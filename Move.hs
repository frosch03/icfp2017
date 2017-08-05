{-# LANGUAGE DeriveDataTypeable #-}

module Move
    ( GamePlay(..)
    , Move(..)
    , SimpleMove(..)
    )
where


import Text.JSON.Generic
import Data.Char

import qualified Score as S
import Auxiliary

type PunterId = Int
type SiteId = Int


data GamePlay
    = GamePlay
      { move :: Move }
    deriving (Show, Data, Typeable)


data Move
    = Move
      { moves :: [SimpleMove]
      }
    | Stop
      { moves :: [SimpleMove]
      , scores :: [S.Score]
      }
    deriving (Show, Data, Typeable)


data SimpleMove
    = Claim
      { punter :: PunterId
      , source :: SiteId
      , target :: SiteId
      }
    | Pass
      { punter :: PunterId
      }
    deriving (Show, Data, Typeable)

