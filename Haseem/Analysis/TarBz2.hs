{-# LANGUAGE
  NoMonomorphismRestriction
  #-}

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


-- _results_glob = "results-???.tar.bz2"


handle_tarball :: Haseem FaH a -> Dir -> Tarball -> Haseem FaH a
handle_tarball haseem (Dir target) tball = do
  wa <- unDir `fmap` getWorkArea
  liftIO $ createDirectoryIfMissing True target
  liftIO $ print target
  liftIO $ sys_extract_tarbz2 tball target
  withWorkArea (Dir $ wa </> target) haseem


tarbz2 :: Haseem FaH a -> Haseem FaH a
tarbz2 haseem = do
  cfg <- getConfig
  wa  <- getWorkArea
  let tb = fahToTarballPath' cfg
  handle_tarball haseem wa tb



-- tarballs :: Dir -> IO [Tarball]
-- tarballs dir = sort `fmap` glob (unDir dir </> _results_glob)

-- expand_dir wa = combine (unDir wa) . takeFileName . dropExtension . dropExtension

-- tarbz2 :: Haseem FaH a -> Haseem FaH [a]
-- tarbz2 haseem = do 
  



  -- tinfo <- getToolInfo
  -- r <- getRunVal
  -- c <- getCloneVal

  -- addLog' $ (printf "starting run %d clone %d" r c :: String)

  -- tarballs <- liftIO $ tarballs (trajArea tinfo)

  -- mapM_ (addLog' . (\tb -> printf "Run %d Clone %d: Found %s " r c (takeFileName tb) :: String) . show) tarballs

  -- let target = expand_dir (workArea tinfo)
  

  -- ret <- mapM (\tb -> handle_tarball tool (target tb) tb)  tarballs

  -- liftIO $ mapM_ (removeDirectoryRecursive . target) tarballs


  -- return ret
