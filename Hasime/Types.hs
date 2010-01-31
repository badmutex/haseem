module Hasime.Types where

import Control.Monad.Writer
import Control.Concurrent.CHP.Monad



class DoConfig a where
    doConfig :: a -> IO b

type Name = String
type ProjectNumber = Integer
type Description = String
type Run = Integer
type Clone = Integer

newtype File = File FilePath deriving (Eq, Ord, Show)
newtype Dir  = Dir  FilePath deriving (Eq, Ord, Show)


data Replace = MkReplace {
      regex :: String
    , replacement :: String
    }

