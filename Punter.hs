{-# LANGUAGE DeriveDataTypeable #-}

module Punter
    ( Setup(..)
    , GSM
    , PunterId
    , GameState(..)
    , Settings(..)
    , gsmIO
    )
where

import Text.JSON.Generic
import Control.Monad.State (StateT, liftIO)
import Text.ParserCombinators.Parsec

import Map 
import Auxiliary 
import Parsers
    

type PunterId = Int

data Settings
    = Settings
      { splurges :: Bool
      , futures  :: Bool
      , options  :: Bool
      } deriving (Show, Data, Typeable)

data Setup
    = Setup
      { punter   :: PunterId
      , punters  :: Int
      , map      :: Map
      , state    :: GameState
      , settings :: Settings
      } deriving (Show, Data, Typeable)

instance Read (Setup) where
    readsPrec p s = case parse pSetup "" s of
                      Left _  -> error $ "error while parsing Setup"
                      Right x -> [(x, "")]

data GameState
    = GameState
      { gamemap   :: Map
      , ownid     :: Int
      , pcount    :: Int
      , unclaimed :: [River]
      , myRivers  :: [River]
      , remaining :: Int
      } deriving (Show, Data, Typeable)




type GSM = StateT GameState IO

gsmIO :: IO a -> GSM a
gsmIO = liftIO

pSetup :: GenParser Char st Setup
pSetup
    = do try ( do char '{'
                  p <- pQuotedInt "punter"
                  char ','
                  n <- pQuotedInt "punters"
                  char ','
                  string "\"map\":"
                  char '{'
                  m  <- pMap
                  char '}'
                  char ','
                  set <- pSettings
                  char ','
                  sta <- pState
                  char '}'
                  return (Setup p n m sta set)
             )
         <|> ( do char '{'
                  p <- pQuotedInt "punter"
                  char ','
                  n <- pQuotedInt "punters"
                  char ','
                  string "\"map\":"
                  char '{'
                  m  <- pMap
                  char '}'
                  char ','
                  sta <- pState
                  char '}'
                  return (Setup p n m sta (Settings False False False))
             )


pState :: GenParser Char st GameState
pState
    = do string "\"state\":"
         char '{'
         char '}'
         return (GameState (Map [] [] []) 0 0 [] [] 0)


pSettings :: GenParser Char st Settings
pSettings
    = try ( do string "\"settings\":"
               char '{'
               sp <- pQuotedBool "splurges"
               char ','
               fu <- pQuotedBool "futures"
               char ','
               op <- pQuotedBool "options"
               char '}'
               return (Settings sp fu op)
          )
      <|>
      try ( do string "\"settings\":"
               char '{'
               fu <- pQuotedBool "futures"
               char ','
               sp <- pQuotedBool "splurges"
               char ','
               op <- pQuotedBool "options"
               char '}'
               return (Settings sp fu op)
          )
      <|>
      try ( do string "\"settings\":"
               char '{'
               sp <- pQuotedBool "splurges"
               char ','
               op <- pQuotedBool "options"
               char ','
               fu <- pQuotedBool "futures"
               char '}'
               return (Settings sp fu op)
          )
      <|>
      try ( do string "\"settings\":"
               char '{'
               op <- pQuotedBool "options"
               char ','
               sp <- pQuotedBool "splurges"
               char ','
               fu <- pQuotedBool "futures"
               char '}'
               return (Settings sp fu op)
          )
      <|>
      try ( do string "\"settings\":"
               char '{'
               op <- pQuotedBool "options"
               char ','
               fu <- pQuotedBool "futures"
               char ','
               sp <- pQuotedBool "splurges"
               char '}'
               return (Settings sp fu op)
          )
      <|>
      try ( do string "\"settings\":"
               char '{'
               fu <- pQuotedBool "futures"
               char ','
               op <- pQuotedBool "options"
               char ','
               sp <- pQuotedBool "splurges"
               char '}'
               return (Settings sp fu op)
          )
      <|>
      try ( do string "\"settings\":"
               char '{'
               sp <- pQuotedBool "splurges"
               char ','
               fu <- pQuotedBool "futures"
               char '}'
               return (Settings sp fu False)
          )
      <|>
      try ( do string "\"settings\":"
               char '{'
               fu <- pQuotedBool "futures"
               char ','
               sp <- pQuotedBool "splurges"
               char '}'
               return (Settings sp fu False)
          )
      <|>
      try ( do string "\"settings\":"
               char '{'
               op <- pQuotedBool "options"
               char ','
               sp <- pQuotedBool "splurges"
               char '}'
               return (Settings sp False op)
          )
      <|>
      try ( do string "\"settings\":"
               char '{'
               sp <- pQuotedBool "splurges"
               char ','
               op <- pQuotedBool "options"
               char '}'
               return (Settings sp False op)
          )
      <|>
      try ( do string "\"settings\":"
               char '{'
               fu <- pQuotedBool "futures"
               char ','
               op <- pQuotedBool "options"
               char '}'
               return (Settings False fu op)
          )
      <|>
      try ( do string "\"settings\":"
               char '{'
               op <- pQuotedBool "options"
               char ','
               fu <- pQuotedBool "futures"
               char '}'
               return (Settings False fu op)
          )
      <|>
      try ( do string "\"settings\":"
               char '{'
               fu <- pQuotedBool "futures"
               char '}'
               return (Settings False fu False)
          )
      <|>
      try ( do string "\"settings\":"
               char '{'
               op <- pQuotedBool "options"
               char '}'
               return (Settings False False op)
          )
      <|>
      try ( do string "\"settings\":"
               char '{'
               sp <- pQuotedBool "splurges"
               char '}'
               return (Settings sp False False)
          )
      <|> ( do return (Settings False False False)
          )


pMap :: GenParser Char st Map
pMap
    = do sts <- pSites
         char ','
         rvs <- pRivers
         char ','
         mns <- pQuotedInts "mines"
         return (Map sts rvs mns)

pRiver :: GenParser Char st River
pRiver
    = do char '{'
         s <- pQuotedInt "source"
         char ','
         t  <- pQuotedInt "target"
         char '}'
         return (River s t)

pRivers :: GenParser Char st [River]
pRivers
    = do string "\"rivers\":"
         char '['
         rivers  <- sepBy pRiver (char ',')
         char ']'
         return rivers

pSite :: GenParser Char st Site
pSite
    = do char '{'
         s <- pQuotedInt "id"
         manyTill anyChar $ (try $ char '}')
         return (Site s)

pSites :: GenParser Char st [Site]
pSites
    = do string "\"sites\":"
         char '['
         sites  <- sepBy pSite (char ',')
         char ']'
         return sites

