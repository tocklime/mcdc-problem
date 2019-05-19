{-# LANGUAGE NoImplicitPrelude #-}
module Types where

import RIO
import RIO.Process

-- | Command line arguments
data Options = Options
  { optionsVerbose :: !Bool
  }

data App = App
  { appLogFunc :: !LogFunc
  , appProcessContext :: !ProcessContext
  , appOptions :: !Options
  -- Add other app-specific configuration information here
  }

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })
instance HasProcessContext App where
  processContextL = lens appProcessContext (\x y -> x { appProcessContext = y })

----------------------------------------------------

data BoolExp a = Lit a
               | And (BoolExp a) (BoolExp a)
               | Or (BoolExp a) (BoolExp a)
               | Not (BoolExp a)
               deriving (Eq, Show)