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

newtype File = File FilePath deriving (Eq, Ord, Show)
newtype Dir  = Dir  FilePath deriving (Eq, Ord, Show)


data Replace = MkReplace {
      regex       :: String
    , replacement :: String
    }

type Logger = MonadIO m => String -> m ()
