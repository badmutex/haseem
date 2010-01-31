{-# LANGUAGE
  Rank2Types
  #-}

module Haseem.Config.Default where

import Haseem.Types
import Haseem.Monad
import Haseem.System.IO


import Control.Monad.Trans
import System.FilePath
import Text.Printf
import HSH
import Prelude hiding (writeFile)


data DefaultConfig = MkDefaultConfig {
      name             :: Name
    , projectNumber    :: ProjectNumber
    , description      :: Description
    , numRuns          :: Run
    , runExplanation   :: String
    , numClones        :: Clone
    , cloneExplanation :: String
    , paramFiles       :: [File]
    , masterConfig     :: File
    , replacements     :: Run -> Clone -> [Replace]
    , executable       :: File
    }


meta :: DefaultConfig -> String
meta conf =
    printf "name: %s\n\
            \number: %d\n\
            \description: %s\n\
            \runs: %d\n\
            \\t%s\n\
            \clones: %d\n\
            \\t%s"
            (name              conf)
            (projectNumber     conf)
            (description       conf)
            (numRuns           conf)
            (runExplanation    conf)
            (numClones         conf)
            (cloneExplanation  conf)


metaFile :: DefaultConfig -> File
metaFile = File . printf "%s.metadata" . name

writeMetadata :: DefaultConfig -> IO ()
writeMetadata conf = liftIO $ 
                     writeFile (metaFile conf) (meta conf ++ "\n")


doDefaultConfig :: DefaultConfig -> IO ()
doDefaultConfig conf = undefined

runs :: DefaultConfig -> [Run]
runs conf = [0..numRuns conf - 1]

clones :: DefaultConfig -> [Clone]
clones conf = [0..numClones conf - 1]

targets :: DefaultConfig -> [Dir]
targets conf = map (uncurry (target (name conf))) rcs
    where rcs = [ (r, c) |
                  r <- runs conf,
                  c <- clones conf
                ]

target :: Name -> Run -> Clone -> Dir
target n r c = Dir $ n </> printf "RUN%d" r </> printf "CLONE%d" c


mkDirs :: DefaultConfig -> IO [Dir]
mkDirs conf = let ts = targets conf
              in do sequence_
                          $ map (liftIO . createDirectoryIfMissing True)
                          $ ts
                    return ts


-- | Each file is copied into the target directory. Assumes that the directories have already been created.
copyFiles :: [File] -> [Dir] -> IO ()
copyFiles fs dirs =
    sequence_    [ liftIO $ copyFileTo f d |
                   f <- fs,
                   d <- dirs
                 ]


specializations :: DefaultConfig -> [(File, [Replace])]
specializations conf = let targetMasterDir r c = target (name conf) r c
                       in [ (copyFileTarget (masterConfig conf) (targetMasterDir r c), replacements conf r c) | 
                            r <- runs conf,
                            c <- clones conf
                          ]

specialize :: [(File, [Replace])] -> IO ()
specialize rs = mapM_ spec rs

    where spec = liftIO . uncurry specializeF

          specialize1 :: File -> Replace -> IO ()
          specialize1 (File f) rep =
              let cmd = printf "sed -i 's/%s/%s/' %s" (regex rep) (replacement rep) f :: String
              in run cmd

          specializeF :: File -> [Replace] -> IO ()
          specializeF f = mapM_ (specialize1 f)


prepare :: DefaultConfig -> IO ()
prepare conf = do
  writeMetadata conf
  ds <- mkDirs conf
  copyFiles (masterConfig conf : paramFiles conf) ds
  specialize . specializations $ conf
