
import Haseem.Types
import Haseem.Monad
import Haseem.Analysis.Config.FaH
import Haseem.Analysis.VMD.RMSD
import Haseem.Analysis.TarBz2

import System.FilePath


proot = "/home/badi/Research/fah/test/data/PROJ10005"
fah = MkFaH {
        projectRoot = Dir proot
      , run         = 6
      , clone       = 0
      , gen         = 0
      }


hcfg = MkHaseemConfig {
         logger   = undefined
       , config   = fah
       , workArea = Dir "/tmp/wa"
       }

vmdcfg = let root = "/home/badi/Research"
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

genparams = genParams vmdcfg

testrmsd = rmsd genparams

test = runHaseem hcfg ((wrapWorkArea . myWorkArea . tarbz2) testrmsd)
