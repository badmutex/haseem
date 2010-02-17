
module Main where

import Haseem.Types
import Haseem.Monad
import Haseem.Analysis.Config.FaH
import Haseem.Analysis.VMD.RMSD
import Haseem.Analysis.TarBz2

import System.Directory
import System.FilePath
import System.Environment
import Data.List
import Text.Printf


main = do
  tarball <- getArgs >>= canonicalizePath . head
  let
      rmsd'     = rmsd genparams
      genparams = genParams vmdcfg

      vmdcfg = let root  = "~/Research/md"
                   ww14  = root </> "ww14/folded/ww_structure_14_model_charm"
                   fip35 = root </> "fip35/folded/ww_folded_nowater1"
               in VMDConfig {
                        vmd_bin      = "vmd"
                      , psfpath      = fip35 <.> "psf"
                      , foldedpath   = fip35 <.> "pdb"
                      , scriptname   = "rmsd.tcl"
                      , resultsname  = "rmsd.txt"
                      , dcdname      = "ww.dcd"
                      , atomselect   = MkAtomSelect "all"
                      , screenoutput = DevNull
                      }

      hcfg = MkHaseemConfig {
               logger   = undefined
             , config   = fromTarball $ File tarball
             , workArea = Dir "/tmp/rmsd/wa"
               }

  rmsds <- runHaseem hcfg ((wrapWorkArea . myWorkArea . tarbz2) rmsd')
  case rmsds of
    Left e -> putStrLn $ "Rmsd calculation failed with: " ++ e
    Right rs -> do let frames    = [0..]
                       results   = zip frames rs
                       ssv       :: (Integer,Double) -> String
                       ssv (f,r) = printf "%i %f" f r
                   mapM_ (putStrLn . ssv) results
