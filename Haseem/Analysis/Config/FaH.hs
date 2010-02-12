module Haseem.Analysis.Config.FaH where

import Haseem.Types
import Haseem.Monad

import Text.ParserCombinators.Parsec

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
    , results     :: File
    } deriving Show


times i f v = let vs = iterate f v
              in vs !! i

digits :: (Num a, Read a) => Parser a
digits = read `fmap` many digit

skipTill :: String -> Parser String
skipTill s = anyChar `manyTill` string s

fahFromTarball :: Parser FaH
fahFromTarball = do
  path <- skipTill "PROJ"
  pid  <- digits :: Parser Integer
  skipTill "RUN"
  r    <- digits
  skipTill "CLONE"
  c    <- digits
  skipTill "results-"
  g    <- digits
  return $ MkFaH {
               projectRoot = Dir $ printf "%sPROJ%d" path pid
             , run         = r
             , clone       = c
             , gen         = g
             , results     = undefined -- set in caller
             }


fromTarball :: File -> FaH
fromTarball (File tb) =
    case parse fahFromTarball [] tb of
      Left e -> error $ printf "Failed to parse '%s' into FaH" tb
      Right fah -> fah { results = File tb }


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
  let wa' = Dir $ wa </> printf "RUN%d" (run fah) </> printf "CLONE%d" (clone fah) </> printf "GEN%d" (gen fah)
  withWorkArea wa' m
