module Haseem.Analysis.VMD.RMSD where

import Haseem.Analysis.Config.FaH
import Haseem.Types
import Haseem.Monad

import Control.Monad.Trans

import Data.List
import System.Process
import System.Exit
import System.FilePath
import System.Posix.Files
import Text.Printf


newtype AtomSelect = MkAtomSelect {unAtomSelect :: String}
newtype Script = MkScript {unScript :: String}
newtype Cmd = MkCmd {unCmd :: String}



data CmdParams = CmdParams {
      vmd, psf, dcd, ref, script, outfile :: FilePath
    , screenout :: String
    } deriving Show

data Output = DevNull | Err2Out | Out2Err | Default

save_script :: FilePath -> Script -> IO ()
save_script p s = writeFile p (unScript s)


mkCmd :: CmdParams -> Cmd
mkCmd p = let cmd = printf "%s -dispdev text -psf %s -dcd %s -f %s < %s %s"
                    (vmd p) (psf p) (dcd p) (ref p) (script p) (screenout p)
          in MkCmd cmd


runCmd :: Cmd -> IO ExitCode
runCmd cmd = do h <- runCommand $ unCmd cmd
                waitForProcess h


cmdFailed :: Cmd -> ExitCode -> Either String ()
cmdFailed _ ExitSuccess = Right ()
cmdFailed cmd (ExitFailure ec) = Left $ printf "%s failed with %d" (unCmd cmd) ec


rmsd_results :: FilePath -> IO [Double]
rmsd_results p = (map read . words) `fmap` readFile p


rmsdScript :: FilePath -> AtomSelect -> Script
rmsdScript outfile atomselect =
    let script = intercalate "\n" $ [
                  ""
                 , "set trajid [molinfo index 0]"
                 , "set refid [molinfo index 1]"

                 , "set outfile %s"
 
                 , "set ref [atomselect $refid \"%s\"]"
                 , "set traj [atomselect $trajid \"%s\"]"
                 , "set n [molinfo $trajid get numframes]"
 
                 , "set f [open $outfile \"w\"]"
                 , "for {set i 0} { $i < $n} {incr i} {"
                 , "    $traj frame $i"
                 , "    set fit [measure fit $ref $traj]"
                 , "    $ref move $fit"
                 , "    set rmsd [measure rmsd $ref $traj]"
                 , "    puts $f \"$rmsd\""
                 , "}"
                 , "close $f"
                 ]
    in MkScript $ printf script outfile (unAtomSelect atomselect) (unAtomSelect atomselect)

data VMDConfig = VMDConfig {
      vmd_bin , psfpath, foldedpath    :: FilePath
    , scriptname, resultsname, dcdname :: String
    , atomselect                       :: AtomSelect
    , screenoutput                     :: Output
    }



genParams :: VMDConfig -> Dir -> (CmdParams,AtomSelect)
genParams cfg wa = (params, atomselect cfg)
    where wa' = unDir wa
          params = CmdParams {
                     vmd     = vmd_bin cfg
                   , psf     = psfpath cfg
                   , dcd     = wa' </> dcdname cfg
                   , ref     = foldedpath cfg
                   , script  = wa' </> scriptname cfg
                   , outfile = wa' </> resultsname cfg
                   , screenout = case screenoutput cfg of
                                   DevNull -> ">/dev/null"
                                   Err2Out -> "2>&1"
                                   Out2Err -> "1>&2"
                                   Default -> ""
                   }

type GenCmdParams = Dir -> (CmdParams,AtomSelect)
type ChooseRemovableFiles = CmdParams -> [FilePath]

-- | Given a results-###.tar.bz2 file, reads calculates the RMSD
-- | relative to a given structure using VMD
rmsdCalculation :: GenCmdParams -> Haseem FaH [Double]
rmsdCalculation genparams = do
  wa <- getWorkArea
  let (params, atomsel) = genparams wa
      cmd               = mkCmd params
  liftIO $ save_script (script params) (rmsdScript (outfile params) atomsel)
  liftIO $ runCmd cmd
  liftIO $ rmsd_results $ outfile params


rmsd :: GenCmdParams -> Haseem FaH [Double]
rmsd genParams = do
  config <- getConfig
  wa     <- getWorkArea
  rmsdCalculation genParams
