module Haseem.System.IO where

import Haseem.Types

import qualified  System.Directory as D
import System.FilePath

import Prelude hiding (writeFile, appendFile)
import qualified Prelude as P

import HSH


writeFile :: File -> String -> IO ()
writeFile (File path) = P.writeFile path

appendFile :: File -> String -> IO ()
appendFile (File path) = P.appendFile path


copyFileTarget :: File -> Dir -> File
copyFileTarget (File f) (Dir d) = File $ d </> (takeFileName f)

copyFileTo :: File -> Dir -> IO File
copyFileTo f@(File f') d@(Dir d') = let (File t) = copyFileTarget f d
                                    in D.copyFile f' t >> return (File t)


createDirectoryIfMissing :: Bool -> Dir -> IO ()
createDirectoryIfMissing b (Dir d) = D.createDirectoryIfMissing b d

