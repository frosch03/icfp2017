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
    = GameState js rs [] (length rs)
    where
      js = decodeJSON $ rightcase s
      rs = rivers . Punter.map $ js


isAdjToSite :: River -> SiteId -> Bool
isAdjToSite (River s t) sid
    = s == sid || t == sid

isAdjTo :: River -> River -> Bool
isAdjTo (River s1 t1) (River s2 t2)
    =   s1 == s2 || s1 == t2
      || t1 == s2 || t1 == t2

isAdjToOneOf :: [River] -> River -> Bool
isAdjToOneOf rivers river
    = foldl (\r n -> (isAdjTo river n) || r) False rivers


unclaimedRiverAtMine :: GameState -> Maybe River
unclaimedRiverAtMine s
    | length mineRs == 0
    = Nothing

    | otherwise
    = Just $ head mineRs
    where
      allMines = mines . Punter.map . setup $ s
      freeRs   = unclaimed s
      mineRs   = [x | x <- freeRs, foldl (\r n -> r || (isAdjToSite x n)) False allMines]
                 

adjacentRiver :: GameState -> Maybe River
adjacentRiver s
    | length goodRs == 0
    = Nothing

    | otherwise
    = Just (head goodRs)
    where
      freeRs = unclaimed $ s
      myRs   = myRivers $ s
      goodRs = [x | x <- freeRs, (isAdjToOneOf myRs) x]


aRiver :: GameState -> River
aRiver = head . unclaimed


nextMove :: GSM (M.SimpleMove) 
nextMove 
    = do s <- get
         let nextAdj = maybe (aRiver s) Prelude.id (adjacentRiver s)
             next    = maybe nextAdj    Prelude.id (unclaimedRiverAtMine s)
             pid  = punter . setup $ s
             m    = M.Claim pid (source next) (target next)
             s'   = s { unclaimed = [x | x <- unclaimed $ s, x /= next]
                      , myRivers  = next : (myRivers s)
                      }
         put s'
         gsmIO $ return m


eliminateMove :: M.SimpleMove -> GSM (M.SimpleMove)
eliminateMove m@(M.Pass _)
    = gsmIO $ return m

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

