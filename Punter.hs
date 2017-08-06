{-# LANGUAGE DeriveDataTypeable #-}

module Punter
    ( Setup(..)
    , GSM
    , PunterId
    , GameState(..)
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
      , state   :: GameState
      } deriving (Show, Data, Typeable)

data GameState
    = GameState
      { gamemap   :: Map
      , ownid     :: Int
      , pcount    :: Int
      , unclaimed :: [River]
      , myRivers  :: [River]
      , remaining :: Int
      } deriving (Show, Data, Typeable)

type GSM = StateT GameState IO

gsmIO :: IO a -> GSM a
gsmIO = liftIO
