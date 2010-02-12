module Haseem.Analysis.TarBz2 where

import Haseem.Types
import Haseem.Monad
import Haseem.Archive
import Haseem.Analysis.Config.FaH

import Control.Monad.Trans

import Data.List (sort)

import System.FilePath
import System.Path.Glob
import System.Directory

import Text.Printf



handle_tarball :: Haseem FaH a -> Dir -> Tarball -> Haseem FaH a
handle_tarball haseem (Dir target) tball = do
  wa <- unDir `fmap` getWorkArea
  liftIO $ createDirectoryIfMissing True target
  liftIO $ sys_extract_tarbz2 tball target
  withWorkArea (Dir $ wa </> target) haseem


tarbz2 :: Haseem FaH a -> Haseem FaH a
tarbz2 haseem = do
  cfg <- getConfig
  wa  <- getWorkArea
  let tb = fahToTarballPath' cfg
  handle_tarball haseem wa tb
