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
      } deriving (Data, Typeable)

instance Show Score where
    show (Score p s)
        = '#':(show p) ++ ": " ++ (show s)
          
                    
