module Haseem.Analysis.Config.FaH where

import Haseem.Types
import Haseem.Monad

import Data.Char
import Data.List
import System.FilePath
import Text.Printf

type Gen = Integer

data FaH = MkFaH {
      projectRoot :: Dir
    , run         :: Run
    , clone       :: Clone
    , gen         :: Gen
    } deriving Show


times i f v = let vs = iterate f v
              in vs !! i


fromTarball :: File -> FaH
fromTarball (File tb) =
    let (gen:clone:run:_) = map (read . filter isDigit) . take 3 . reverse . map (times 2 takeBaseName) 
                            $ splitDirectories tb
    in MkFaH {
             projectRoot = Dir . times 3 takeDirectory $ tb
           , run = run
           , clone = clone
           , gen = gen
           }

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
