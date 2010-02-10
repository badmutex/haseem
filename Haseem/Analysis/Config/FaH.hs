module Haseem.Analysis.Config.FaH where

import Haseem.Types
import Haseem.Monad

import System.FilePath
import Text.Printf

type Gen = Integer

data FaH = MkFaH {
      projectRoot :: Dir
    , run         :: Run
    , clone       :: Clone
    , gen         :: Gen
    } deriving Show



fahToTarballPath' :: FaH -> FilePath
fahToTarballPath' fah = printf "%s" (unDir $ projectRoot fah) </>
                        printf "RUN%d" (run fah)              </>
                        printf "CLONE%d" (clone fah)          </>
                        printf "results-%03d.tar.bz2" (gen fah)

fahToTarballPath :: FaH -> File
fahToTarballPath fah = File $ fahToTarballPath' fah

myWorkArea :: Dir -> FaH -> Dir
myWorkArea wa fah = Dir $ (unDir wa) </> takeDirectory (fahToTarballPath' fah)
