{-# LANGUAGE DeriveDataTypeable #-}

module Punter
    ( Setup(..)
    , GameState(..)
    , GSM
    , PunterId
    , gsmIO
    )
where

import Text.JSON.Generic
import Control.Monad.State

import qualified Move as M
import Map 
import Auxiliary 


type PunterId = Int

data Setup
    = Setup
      { punter  :: PunterId
      , punters :: Int
      , map     :: Map
      } deriving (Show, Data, Typeable)

data GameState
    = GameState
      { setup :: Setup
      , unclaimed :: [River]
      , remaining :: Int
      } deriving (Show, Data, Typeable)


type GSM = StateT GameState IO

gsmIO :: IO a -> GSM a
gsmIO = liftIO
