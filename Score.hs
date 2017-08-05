{-# LANGUAGE DeriveDataTypeable #-}

module Score
    ( Score(..)
    )
where


import Text.JSON.Generic

type PunterId = Int

data Score
    = Score
      { punter :: PunterId
      , score  :: Int
      } deriving (Show, Data, Typeable)

