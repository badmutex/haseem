{-# LANGUAGE
  GeneralizedNewtypeDeriving
  , ImpredicativeTypes
  , Rank2Types
  #-}

module Haseem.Monad where

import Haseem.Types

import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.Writer

import System.Directory

class HConfig a where
    hconfig :: a -> Haseem a b


data HaseemConfig c = MkHaseemConfig {
      logger :: Logger
    , config :: c
    , workArea :: Dir
    }


newtype Haseem c a = MkHaseem {
      runH :: ErrorT String (ReaderT (HaseemConfig c) IO) a
    } deriving (Functor, Monad, MonadError String, MonadReader (HaseemConfig c), MonadIO)


runHaseem :: HaseemConfig c -> Haseem c a -> IO (Either String a)
runHaseem hconfig haseem = flip runReaderT hconfig . runErrorT $ runH haseem


getConfig :: Haseem c c
getConfig = config `liftM` ask

getWorkArea :: Haseem c Dir
getWorkArea = workArea `liftM` ask

withWorkArea :: Dir -> Haseem c a -> Haseem c a
withWorkArea wa h = local f h
    where f hc = hc { workArea = wa }


-- | Creates a workarea, runs the monad in the workarea, then cleans up
wrapWorkArea :: Show a => Dir -> Haseem c a -> Haseem c a
wrapWorkArea wa haseem = do
  liftIO $ createDirectoryIfMissing True (unDir wa)
  h <- haseem
  liftIO $ removeDirectoryRecursive (unDir wa)
  return h