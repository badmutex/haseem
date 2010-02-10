{-# LANGUAGE
  Rank2Types
  #-}

module Haseem.Types where

import Control.Monad.Trans

type Name          = String
type ProjectNumber = Integer
type Description   = String
type Run           = Integer
type Clone         = Integer

newtype File = File {unFile :: FilePath} deriving (Eq, Ord, Show)
newtype Dir  = Dir  {unDir :: FilePath} deriving (Eq, Ord, Show)


data Replace = MkReplace {
      regex       :: String
    , replacement :: String
    }

type Logger = MonadIO m => String -> m ()
