{-# LANGUAGE DeriveDataTypeable #-}

module Protocol
    ( Name(..)
    , Ready(..)
    , pickle
    , unpickle
    )
where

import Text.JSON.Generic

import qualified Move as M
import Map 
import Auxiliary 


data Name
    = Name
      { me :: String
      } deriving (Show, Data, Typeable)

data Ready
    = Ready
      { ready :: Int
      } deriving (Show, Data, Typeable)

pickle :: String -> String
pickle s
    = show l ++ ':':s'
    where
      s' = s ++ "\n"
      l  = length s'

unpickle :: String -> String
unpickle = tail . dropWhile (\x -> x /= ':')


