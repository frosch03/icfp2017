{-# LANGUAGE DeriveDataTypeable #-}

module Game
    ( Setup(..)
    , GameState(..)
    , PunterId
    , initialize
    , gsmIO
    , nextMove
    , eliminateMove
    )
where

import Text.JSON.Generic
import Control.Monad.State

import qualified Move as M
import Map 
import Auxiliary
import Punter


initialize :: String -> GameState
initialize s
    = GameState js rs (length rs)
    where
      js = decodeJSON $ rightcase s
      rs = rivers . Punter.map $ js

nextMove :: GSM (M.SimpleMove) 
nextMove
    = do s <- get
         let free = head . unclaimed $ s
             pid  = punter . setup $ s
             m    = M.Claim pid (source free) (target free)
             s'   = s { unclaimed = tail . unclaimed $ s
                      , remaining = remaining s - 1
                      }
         put s'
         gsmIO $ return m


eliminateMove :: M.SimpleMove -> GSM (M.SimpleMove)
eliminateMove m@(M.Claim _ src tgt)
    = do s <- get
         let freeRivers  = unclaimed s
             freeRivers' = [x | x <- freeRivers, x /= claimedRiver]
             s'          = s { unclaimed = freeRivers'
                             , remaining = remaining s - 1
                             }
         put s'
         gsmIO $ return m
    where
      claimedRiver = River src tgt

