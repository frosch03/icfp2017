{-# LANGUAGE DeriveDataTypeable #-}

module Game
    ( Setup(..)
    , PunterId
    , initialize
    , gsmIO
    , nextMove
    , eliminateMove
    )
where

import Text.JSON.Generic
import Control.Monad.State
import Data.List

import qualified Move as M
import Map 
import Auxiliary
import Punter


initialize :: String -> GameState
initialize s
    = GameState (Punter.map js) (punter js) (punters js) rs [] (length rs)
    where
      js = read s
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


connectedSites :: GameState -> [SiteId]
connectedSites s
    = nub sites
    where
      myRs  = myRivers s
      sites = foldr (\n r -> (source n):(target n):r) [] myRs

riversAtClaimed :: GameState -> [River]
riversAtClaimed s
    = [x | x <- freeRs, foldl (\r n -> r || (isAdjToSite x n)) False connSites]
    where
      connSites = connectedSites s
      freeRs    = unclaimed $ s



riverAtClaimed :: GameState -> Maybe River
riverAtClaimed s
    | length (riversAtClaimed s) == 0
    = Nothing

    | otherwise
    = Just $ head . riversAtClaimed $ s


riverAtClaimedTip :: GameState -> Maybe River
riverAtClaimedTip s
    | length tipRs == 0
    = Nothing

    | otherwise
    = Just $ head tipRs
    where
      tipRs = [x | x <-  rsAtC, cond x]
      rsAtC = riversAtClaimed s
      css   = connectedSites s
      cond x =   (any (== (source x)) css && any (/= (target x)) css)
               || (any (== (target x)) css && any (/= (source x)) css)

          
                 
firstRiverAtMine :: GameState -> Maybe River
firstRiverAtMine s
    | length myRsAtMine > 0
    = Nothing

    | otherwise
    = riverAtMine s
    where
      myRs       = myRivers s
      mineSites  = mines . gamemap $ s
      myRsAtMine = [x | x <- myRs, cond x]
      cond x = any (== (source x)) mineSites || any (== (target x)) mineSites
      


riverAtMine :: GameState -> Maybe River
riverAtMine s
    | length mineRs == 0
    = Nothing

    | otherwise
    = Just $ head mineRs
    where
      allMines = mines . Punter.gamemap $ s
      freeRs   = unclaimed $ s
      mineRs   = [x | x <- freeRs, foldl (\r n -> r || (isAdjToSite x n)) False allMines]
                 

adjacentRiver :: GameState -> Maybe River
adjacentRiver s
    | length goodRs == 0
    = Nothing

    | otherwise
    = Just (head goodRs)
    where
      freeRs = unclaimed $ s
      myRs   = myRivers  $ s
      goodRs = [x | x <- freeRs, (isAdjToOneOf myRs) x]


aRiver :: GameState -> River
aRiver = head . unclaimed


nextMove :: GSM (M.SimpleMove) 
nextMove 
    = do s <- get
         let next = nextAtTip
             nextAtTip     = maybe (nextAtClaimed) Prelude.id (riverAtClaimedTip s)
             nextAtClaimed = maybe (nextAtMine)    Prelude.id (riverAtClaimed s)
             nextAtMine    = maybe (nextAdjacent)  Prelude.id (firstRiverAtMine s)
             nextAdjacent  = maybe (nextAnyRiver)  Prelude.id (adjacentRiver s)
             nextAnyRiver  = aRiver s
             pid  = ownid $ s
             sm   = M.Claim pid (source next) (target next)
             s'   = s { unclaimed = [x | x <- unclaimed $ s, x /= next]
                      , myRivers  = next : (myRivers $ s)
                      }
         put s'
         gsmIO $ return sm


eliminateMove :: M.SimpleMove -> GSM (M.SimpleMove)
eliminateMove m@(M.Pass _)
    = gsmIO $ return m

eliminateMove m@(M.Claim _ src tgt)
    = do s <- get
         let freeRivers  = unclaimed $ s
             freeRivers' = [x | x <- freeRivers, x /= claimedRiver]
             s'          = s { unclaimed = freeRivers'
                             , remaining = (remaining $ s) - 1
                             }
         put s'
         gsmIO $ return m
    where
      claimedRiver = River src tgt

