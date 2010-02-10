{-# LANGUAGE
  Rank2Types
  #-}

module Haseem.Monad where

import Haseem.Types

import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.Writer

class HConfig a where
    hconfig :: a -> Haseem a b


data HConfig c =>
    HaseemConfig c = MkHaseemConfig {
                       logger :: Logger
                     , config :: c
                     }


newtype HConfig c =>
    Haseem c a = MkHaseem {
                   runH :: ErrorT String (ReaderT (HaseemConfig c) IO) a
                 }


runHaseem :: HConfig c =>
             HaseemConfig c -> Haseem c a -> IO (Either String a)
runHaseem hconfig haseem = flip runReaderT hconfig . runErrorT $ runH haseem

