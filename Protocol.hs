{-# LANGUAGE DeriveDataTypeable #-}

module Protocol
    ( Name(..)
    , Ready(..)
    , Messages(..)
    , pickle
    , unpickle
    , netResult
    )
where

import Text.JSON.Generic
import Text.ParserCombinators.Parsec

import Control.Applicative ((<$>))

import qualified Move as M
import Map
import Punter
import Auxiliary 


data Name
    = Name
      { me :: String
      } deriving (Show, Data, Typeable)

data Ready
    = Ready
      { ready :: Int
      , state :: GameState
      } deriving (Show, Data, Typeable)


data Messages
    = Messages [(Int, String)]

instance Read Messages where
    readsPrec p s = case parse netResult "" s of
                      Left _  -> error $ "error while parsing Message"
                      Right x -> [(Messages x, "")]
      

netResult :: GenParser Char st [(Int, String)]
netResult
    = do result <- many message
         eol
         return result


pickle :: String -> String
pickle s
    = show l ++ ':':s'
    where
      s' = s ++ "\n"
      l  = length s'

unpickle :: String -> String
unpickle = tail . dropWhile (\x -> x /= ':')




data Tree = Leaf String | Node [Tree]

parseTree :: GenParser Char st Tree
parseTree = node <|> leaf
  where
    node = Node <$> between (char '{') (char '}') (many parseTree)
    leaf = Leaf <$> many1 (noneOf "{}")

nodes :: Tree -> [Tree]
nodes (Leaf _) = []
nodes t@(Node ts) = t : concatMap nodes ts

instance Show Tree where
    showsPrec d (Leaf x) = showString x
    showsPrec d (Node xs) = showString "{" . showList xs . showString "}"
        where
          showList [] = Prelude.id
          showList (x:xs) = shows x . showList xs


message :: GenParser Char st (Int, String)
message
    = do len    <- prefix
         char ':'
         msgtree <- parseTree
         return (len, show msgtree)
    
remainingMsg :: GenParser Char st String
remainingMsg
    = between (char '{') (char '}') (many anyChar)

prefix :: GenParser Char st (Int)
prefix
    = do len <- many digit
         return (read len)

eol :: GenParser Char st Char
eol = char '\n'
