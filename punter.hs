{-# LANGUAGE DeriveDataTypeable #-}

module Punter
where

import Text.JSON.Generic
import Control.Monad.State
import Network
import System.IO


import qualified Move as M


type PunterId = Int
type SiteId = Int

data Site
    = Site
      { id :: SiteId
      } deriving (Show, Data, Typeable)

data River
    = River
      { source :: SiteId
      , target :: SiteId
      } deriving (Show, Data, Typeable)

instance Eq River where
    (River s1 t1) == (River s2 t2)
        = (s1 == s2 && t1 == t2) || (s1 == t2 && t1 == s2)

data Map
    = Map
      { sites  :: [Site]
      , rivers :: [River]
      , mines  :: [SiteId]
      } deriving (Show, Data, Typeable)

data Setup
    = Setup
      { punter  :: PunterId
      , punters :: Int
      , map     :: Map
      } deriving (Show, Data, Typeable)

data GameState
    = GameState
      { setup :: Setup
      , unclaimed :: [River]
      , remaining :: Int
      } deriving (Show, Data, Typeable)



initialize :: String -> GameState
initialize s
    = GameState js rs (length rs)
    where
      js = decodeJSON s
      rs = rivers . Punter.map $ js


type GSM = StateT GameState IO

gsmIO :: IO a -> GSM a
gsmIO = liftIO

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


dropLength :: String -> String
dropLength = tail . dropWhile (\x -> x /= ':')


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

dropFirstLast :: String -> String
dropFirstLast = tail . reverse . tail . reverse

main :: IO ()
main
    = do h <- connectTo "punter.inf.ed.ac.uk" (PortNumber 9009)
         hPutStr h (pickle . encodeJSON $ Name "frosch03")
         _ <- hGetLine h

         l <- hGetLine h >>= (\x -> return $ dropLength x)
         putStrLn ("GameState received")

         let s = initialize l
             p = punter . setup $ s

         putStrLn ("Punter ID:" ++ show p)
         hPutStr h (pickle . encodeJSON $ Ready p)
         l <- hGetLine h >>= (\x -> return $ dropLength x)
         -- (m, s') <- runStateT nextMove s

         return ()
