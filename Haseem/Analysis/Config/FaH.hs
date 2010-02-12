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


relativeRunCloneGenTarball :: FaH -> FilePath
relativeRunCloneGenTarball fah = printf "RUN%d" (run fah)              </>
                                 printf "CLONE%d" (clone fah)          </>
                                 printf "results-%03d.tar.bz2" (gen fah)

fahToTarballPath' :: FaH -> FilePath
fahToTarballPath' fah = printf "%s" (unDir $ projectRoot fah) </>
                        relativeRunCloneGenTarball fah

fahToTarballPath :: FaH -> File
fahToTarballPath fah = File $ fahToTarballPath' fah

myWorkArea :: Haseem FaH a -> Haseem FaH a
myWorkArea m = do
  wa  <- unDir `fmap` getWorkArea
  fah <- getConfig
  let wa' = Dir $ wa </> takeDirectory (relativeRunCloneGenTarball fah)
  withWorkArea wa' m
