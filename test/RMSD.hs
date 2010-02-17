
module Main where

import Haseem.Types
import Haseem.Monad
import Haseem.Analysis.Config.FaH
import Haseem.Analysis.VMD.RMSD
import Haseem.Analysis.TarBz2

import Control.Applicative ((<$>))
import System.Directory
import System.FilePath
import System.Environment
import Data.List
import Text.Printf


type Frame = Integer

rcg :: Show a => Haseem FaH [a] -> Haseem FaH [(Run, Clone, Gen, Frame, a)]
rcg h = do
  rmsds <- h
  fah   <- getConfig
  let [rs, cs, gs] = map (repeat . flip ($) fah) [run, clone, gen]
      frames       = zip5 rs cs gs [0..] rmsds
  return frames


csv :: (Run, Clone, Gen, Frame, Double) -> String
csv (r,c,g,f,rmsds) = printf "%i,%i,%i,%i,%f" r c g f rmsds


main = do
  cwd     <- getCurrentDirectory
  tarball <- (</>) cwd . head <$> getArgs

  let
      rmsd'     = rmsd genparams
      genparams = genParams vmdcfg

      vmdcfg = let root  = "/afs/crc.nd.edu/user/c/cabdulwa/Research/md"
                   ww14  = root </> "ww14/folded/ww_structure_14_model_charm"
                   fip35 = root </> "fip35/folded/ww_folded_nowater1"
               in VMDConfig {
                        vmd_bin      = "/afs/crc.nd.edu/x86_64_linux/vmd/1.8.6/bin/vmd"
                      , psfpath      = fip35 <.> "psf"
                      , foldedpath   = fip35 <.> "pdb"
                      , scriptname   = "rmsd.tcl"
                      , resultsname  = "rmsd.txt"
                      , dcdname      = "ww.dcd"
                      , atomselect   = MkAtomSelect "protein and resid 11 to 16 21 to 26 30 to 33 and name CA"
                      , screenoutput = DevNull
                      }

      hcfg = MkHaseemConfig {
               logger   = undefined
             , config   = fromTarball $ File tarball
             , workArea = Dir $ cwd </> "wa"
             }

  rmsds <- runHaseem hcfg ((myWorkArea . tarbz2) (rcg rmsd'))

  case rmsds of
    Left e   -> putStrLn $ "Rmsd calculation failed with: " ++ e
    Right rs -> mapM_ (putStrLn . csv) rs
