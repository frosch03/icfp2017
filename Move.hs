{-# LANGUAGE DeriveDataTypeable #-}

module Move
    (  Move(..)
    , SimpleMove(..)
    , isStopped
    )
where


import Text.JSON.Generic
import Data.Char

import qualified Score as S
import Auxiliary
import Map (SiteId)

type PunterId = Int

isStopped :: Move -> Bool
isStopped (Move _)   = False
isStopped (Stop _ _) = True

data Move
    = Move
      { moves :: [SimpleMove]
      }
    | Stop
      { moves :: [SimpleMove]
      , scores :: [S.Score]
      }
    deriving (Data, Typeable)


instance Show Move where
    show (Move sms)
        = "[" ++ (foldr (\n r -> (show n) ++ ", " ++ r) "]" sms)

    show (Stop sms scs)
        =    "Game Stopped\n\n"
          ++ "Last moves:" ++ "[" ++ (foldr (\n r -> (show n) ++ ", " ++ r) "]\n" sms)
          ++ "Scores:    " ++ "(" ++ (foldr (\n r -> (show n) ++ ", " ++ r) ")" scs)


data SimpleMove
    = Claim
      { punter :: PunterId
      , source :: SiteId
      , target :: SiteId
      }
    | Pass
      { punter :: PunterId
      }
    deriving (Data, Typeable)

instance Show SimpleMove where
    show (Claim p s t)
        = (show p) ++ ": " ++
          (show s) ++ "--" ++ (show t)

    show (Pass p)
        = (show p) ++ ": Pass"
