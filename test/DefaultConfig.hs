
import Haseem.Types
import Haseem.Monad
import Haseem.Config.Default

import Control.Monad.Trans
import Control.Concurrent
import System.FilePath
import Control.Concurrent.CHP hiding ((</>))


test = prepare conf
          
    where conf = MkDefaultConfig {
                   name             = "test/testproj"
                 , projectNumber    = 42
                 , description      = "description here"
                 , numRuns          = 9
                 , runExplanation   = "runs explanation"
                 , numClones        = 1
                 , cloneExplanation = "clones explanation"
                 , paramFiles       = map (File . (</>) "/home/badi/src/haseem.git/test/CLONE0")
                                      [ "par_all27_prot_lipid.inp", "scpismQuartic.inp"
                                      , "ww_folded_min.pdb", "ww_exteq_nowater1.psf"]
                 , masterConfig     = File "/home/badi/src/haseem.git/test/ww.md.conf"
                 , replacements     = \_ _ -> [MkReplace { regex = " SETSEED ", replacement = "42" }]
                 , executable       = File "ProtoMol"
                 }
