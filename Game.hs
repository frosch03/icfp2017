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
    = GameState (Punter.map js) (punter js) (punters js) rs [] rs [] (length rs) (length ms)
    where
      js = read s
      rs = rivers . Punter.map $ js
      ms = mines  . Punter.map $ js


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
      myRs  = myclaimed s
      sites = foldr (\n r -> (source n):(target n):r) [] myRs


myRiversAtSite :: GameState -> SiteId -> [River]
myRiversAtSite s sid
    = [x | x <- myclaimed s, sid == (source x) || sid == (target x)]

freeRiversAtSite :: GameState -> SiteId -> [River]
freeRiversAtSite s sid
    = [x | x <- unclaimed s, sid == (source x) || sid == (target x)]

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
      tipRs = [x | x <- riversAtClaimed s, cond x]
      myRivCntAt sid = length $ myRiversAtSite s sid
      cond x =   myRivCntAt (source x) == 0 && myRivCntAt (target x) == 1
               || myRivCntAt (source x) == 1 && myRivCntAt (target x) == 0

          
                 
firstRiverAtMine :: GameState -> Maybe River
firstRiverAtMine s
    | length myRsAtMine > 0
    = Nothing

    | otherwise
    = riverAtMine s
    where
      myRs       = myclaimed s
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
      myRs   = myclaimed  $ s
      goodRs = [x | x <- freeRs, (isAdjToOneOf myRs) x]


aRiver :: GameState -> River
aRiver = head . unclaimed


nextMove :: GSM (M.SimpleMove) 
nextMove 
    = do s <- get
         let next = nextAtTip
             nextAtTip     = maybe (nextAtMine)    Prelude.id (riverAtClaimedTip s)
             nextAtMine    = maybe (nextAtClaimed) Prelude.id (firstRiverAtMine s)
             nextAtClaimed = maybe (nextAdjacent)  Prelude.id (riverAtClaimed s)
             nextAdjacent  = maybe (nextAnyRiver)  Prelude.id (adjacentRiver s)
             nextAnyRiver  = aRiver s
             pid  = ownid $ s
             sm   = M.Claim pid (source next) (target next)
             s'   = s { unclaimed = [x | x <- unclaimed $ s, x /= next]
                      , myclaimed  = next : (myclaimed $ s)
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
             s' = s { unclaimed = freeRivers'
                    , remaining = (remaining $ s) - 1
                    }
         put s'
         gsmIO $ return m
    where
      claimedRiver = River src tgt

eliminateMove m@(M.Splurge _ sids)
    = do s <- get
         let claimedRivers = [(River s t) | (s, t) <- zip sids (tail sids)]
             freeRivers    = unclaimed s
             freeRivers'   = [x | x <- freeRivers, any (/= x) claimedRivers]
             s' = s { unclaimed = freeRivers
                    , remaining = (remaining $ s) - 1
                    }
         put s'
         gsmIO $ return m

eliminateMove m@(M.Option _ src tgt)
    = do s <- get
         let freeOptions  = unopted $ s
             freeOptions' = [x | x <- freeOptions, x /= optedRiver]
             s' = s { unclaimed = freeOptions'
                    , opcredit = (opcredit $ s) - 1
                    }
         put s'
         gsmIO $ return m
    where
      optedRiver = River src tgt
