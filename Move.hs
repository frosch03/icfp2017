{-# LANGUAGE DeriveDataTypeable #-}

module Move
    ( Move(..)
    , SimpleMove(..)
    )
where


import Text.JSON.Generic

type PunterId = Int
type SiteId = Int


data Move
    = Move
      { moves :: [SimpleMove]
      } deriving (Show, Data, Typeable)

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


    
