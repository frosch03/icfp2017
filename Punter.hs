{-# LANGUAGE DeriveDataTypeable #-}

module Punter
    ( Setup(..)
    , GameState(..)
    , GSM
    , PunterId
    , MyState(..)
    , gsmIO
    )
where

import Text.JSON.Generic
import Control.Monad.State (StateT, liftIO)

import Map 
import Auxiliary 


type PunterId = Int

data Setup
    = Setup
      { punter  :: PunterId
      , punters :: Int
      , map     :: Map
      , state   :: MyState
      } deriving (Show, Data, Typeable)

data MyState
    = MyState
      { gamemap   :: Map
      , ownid     :: Int
      , pcount    :: Int
      , unclaimed :: [River]
      , myRivers  :: [River]
      , remaining :: Int
      } deriving (Show, Data, Typeable)

data GameState
    = GameState
      { setup   :: Setup
      , mystate :: MyState
      } deriving (Show, Data, Typeable)


type GSM = StateT MyState IO

gsmIO :: IO a -> GSM a
gsmIO = liftIO
