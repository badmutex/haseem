module Main where

import Haseem.Types
import Haseem.Monad
import Haseem.Analysis.Config.FaH
import Haseem.Analysis.VMD.RMSD
import Haseem.Analysis.TarBz2

import System.FilePath
import System.Environment
import Data.List
import Text.Printf


main = do
  [tarball] <- getArgs
  let
      rmsd' = rmsd genparams
      genparams = genParams vmdcfg

      vmdcfg = let root = "~/Research"
               in VMDConfig {
                        vmd_bin      = "vmd"
                      , psfpath      = root </> "md/ww14/folded/ww_structure_14_model_charm.psf"
                      , foldedpath   = root </> "md/ww14/folded/ww_structure_14_model_charm.pdb"
                      , scriptname   = "rmsd.tcl"
                      , resultsname  = "rmsd.txt"
                      , dcdname      = "ww.dcd"
                      , atomselect   = MkAtomSelect "all"
                      , screenoutput = DevNull
                      }

      hcfg = MkHaseemConfig {
               logger   = undefined
             , config   = fromTarball $ File tarball
             , workArea = Dir "/tmp/test/wa"
               }

  (Right rmsds) <- runHaseem hcfg ((wrapWorkArea . myWorkArea . tarbz2) rmsd')
  let frames = [0..]
      results = zip frames rmsds
      ssv :: (Integer,Double) -> String
      ssv (f,r) = printf "%i %f" f r
  mapM_ (putStrLn . ssv) results
