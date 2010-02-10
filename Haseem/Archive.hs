
module Haseem.Archive where

import Haseem.Types

import Control.Applicative    ((<$>))
import Codec.Compression.BZip (decompress)
import Text.Printf            (printf)

import System.Process
import System.Exit


type Tarball   = FilePath
type TargetDir = FilePath


sys_extract_tarbz2 :: Tarball -> TargetDir -> IO  ExitCode
sys_extract_tarbz2 tarball targetdir = do
  let cmd = printf "cd %s; tar jxf %s" targetdir tarball
  waitForProcess =<< runCommand cmd
