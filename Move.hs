{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Move
    ( Move(..)
    , SimpleMove(..)
    , TmpShow(..)
    , isStopped
    , getSimpleMoves
    )
where


import Text.ParserCombinators.Parsec
import Text.JSON.Generic
import Data.Char


import qualified Score as S
import Auxiliary
import Map (SiteId, Map(..), Site(..), River(..))
import Punter (GameState(..))
import Parsers 

type PunterId = Int

colors :: [String]
colors = ["#800000", "#ff0000", "#808000", "#ffff00", "#008000", "#00ff00", "#008080", "#404040", "#00ffff", "#000080", "#0000ff", "#000000", "#800080", "#a0a0a0", "#ff00ff", "#f0f00f0"]


getSimpleMoves :: Move -> [SimpleMove]
getSimpleMoves (Move sm _) = sm
getSimpleMoves (Stop sm _) = sm

isStopped :: Move -> Bool
isStopped (Move _ _) = False
isStopped (Stop _ _) = True

data Move
    = Move
      { moves :: [SimpleMove]
      , state :: GameState
      }
    | Stop
      { moves :: [SimpleMove]
      , scores :: [S.Score]
      }
    deriving (Data, Typeable)


instance Show Move where
    show (Move sms s)
        = foldr (\n r -> "    " ++ (show (TmpShow i n)) ++ "\n" ++ r) "" sms
        where
          i = remaining s

    show (Stop sms scs)
        = (foldr (\n r -> "    " ++ (show (TmpShow 0 n)) ++ "\n" ++ r) "" sms)
          ++ "}\n"

instance Read (Move) where
    readsPrec p s = case parse pMove "" s of
                      Left _  -> error $ "error while parsing Move"
                      Right x -> [(x, "")]


data SimpleMove
    = Claim
      { punter :: PunterId
      , source :: SiteId
      , target :: SiteId
      }
    | Pass
      { punter :: PunterId
      }
    | Splurge
      { punter :: PunterId
      , route  :: [SiteId]
      }
    | Option
      { punter :: PunterId
      , source :: SiteId
      , target :: SiteId
      }
    deriving (Data, Typeable)




data TmpShow 
    = TmpShow
      { tmpRemaining :: Int
      , tmpTheMove   :: SimpleMove
      } 

instance Show TmpShow where
    show (TmpShow i (Claim p s t))
        = "q" ++ (show s) ++ "--q" ++ (show t) ++
          " [ color = \"" ++ colors!!p ++
          "\", penwidth = \"3\", label = \"" ++ (show p) ++ ", " ++
                                              (show i) ++ "\" ];"

    show (TmpShow i (Pass p))
        = ""

    show (TmpShow i (Splurge p r))
        = concat $ zipWith (\s t -> (show (TmpShow i (Claim p s t)))) r $ tail r

    show (TmpShow i (Option p s t) )
        = "q" ++ (show s) ++ "--q" ++ (show t) ++
          " [ color = \"" ++ (colors!!p) ++
          "\", penwidth = \"2\", label = \"" ++ (show p) ++ ", " ++
                                              (show i) ++ "\" ];"


             


pMove :: GenParser Char st Move 
pMove
    = do try ( do char '{'
                  string "\"move\":"
                  char '{'
                  sms <- pSimpleMoves
                  char '}'
                  char ','
                  string "\"state\":"
                  char '{'
                  st  <- pGameState
                  char '}'
                  char '}'
                  return (Move sms st)
             )
         <|>
         try ( do char '{'
                  string "\"stop\":"
                  char '{'
                  sms <- pSimpleMoves
                  char ','
                  scs <- pScores
                  char '}'
                  char ','
                  string "\"state\":"
                  char '{'
                  st  <- pGameState
                  char '}'
                  char '}'
                  return (Stop sms scs)

               )


             
pScores :: GenParser Char st [S.Score]
pScores
    = do string "\"scores\":"
         char '['
         sms  <- sepBy pScore (char ',')
         char ']'
         return sms


pScore :: GenParser Char st S.Score
pScore
    = do try ( do char '{'
                  p <- pPunter
                  char ','
                  s <- pQuotedInt "score"
                  char '}'
                  return (S.Score p s)
             )


pSimpleMoves :: GenParser Char st [SimpleMove]
pSimpleMoves
    = do string "\"moves\":"
         char '['
         sms  <- sepBy pSimpleMove (char ',')
         char ']'
         return sms

         

pSimpleMove :: GenParser Char st SimpleMove
pSimpleMove
    = do try ( do char '{'
                  string "\"claim\":"
                  char '{'
                  p <- pPunter
                  char ','
                  s <- pSource
                  char ','
                  t <- pTarget
                  char '}'
                  char '}'
                  return (Claim p s t)
             )
         <|>
         try ( do char '{'
                  string "\"pass\":"
                  char '{'
                  p <- pPunter
                  char '}'
                  char '}'
                  return (Pass p)
             )
         <|>
         try ( do char '{'
                  string "\"splurge\":"
                  char '{'
                  p <- pPunter
                  char ','
                  r <- pRoute
                  char '}'
                  char '}'
                  return (Splurge p r)
             )
         <|> ( do char '{'
                  string "\"option\":"
                  char '{'
                  p <- pPunter
                  char ','
                  s <- pSource
                  char ','
                  t <- pTarget
                  char '}'
                  char '}'
                  return (Option p s t)
             )

               
pRoute :: GenParser Char st [Int]
pRoute = pQuotedInts "route"

pPunter :: GenParser Char st Int
pPunter = pQuotedInt "punter"
          
pSource :: GenParser Char st Int
pSource = pQuotedInt "source"
          
pTarget :: GenParser Char st Int
pTarget = pQuotedInt "target"


pGameMap :: GenParser Char st Map
pGameMap
    = do string "\"gamemap\":"
         char '{'
         ss <- pSites
         char ','
         rs <- pRivers
         char ','
         ms <- pMines
         char '}'
         return (Map ss rs ms)

pSites :: GenParser Char st [Site]
pSites
    = do string "\"sites\":"
         char '['
         sites  <- sepBy pSite (char ',')
         char ']'
         return sites

pMines :: GenParser Char st [Int]
pMines = pQuotedInts "mines"

pOwnId :: GenParser Char st Int
pOwnId = pQuotedInt "ownid"
         
pPcount :: GenParser Char st Int
pPcount = pQuotedInt "pcount"
          
pSite :: GenParser Char st Site
pSite
    = do char '{'
         s <- pQuotedInt "id"
         char '}'
         return (Site s)

pRiver :: GenParser Char st River
pRiver
    = do char '{'
         s <- pSource
         char ','
         t  <- pTarget
         char '}'
         return (River s t)

pRivers :: GenParser Char st [River]
pRivers
    = do string "\"rivers\":"
         char '['
         rivers  <- sepBy pRiver (char ',')
         char ']'
         return rivers

pUnclaimed :: GenParser Char st [River]
pUnclaimed
    = do string "\"unclaimed\":"
         char '['
         rivers  <- sepBy pRiver (char ',')
         char ']'
         return rivers


pMyclaimed :: GenParser Char st [River]
pMyclaimed 
    = do string "\"myclaimed\":"
         char '['
         rivers  <- sepBy pRiver (char ',')
         char ']'
         return rivers


pUnOpted :: GenParser Char st [River]
pUnOpted 
    = do string "\"unopted\":"
         char '['
         rivers  <- sepBy pRiver (char ',')
         char ']'
         return rivers


pMyOpted :: GenParser Char st [River]
pMyOpted  
    = do string "\"myopted\":"
         char '['
         rivers  <- sepBy pRiver (char ',')
         char ']'
         return rivers


pRemaining :: GenParser Char st Int
pRemaining = pQuotedInt "remaining"

pOptionCredit :: GenParser Char st Int
pOptionCredit = pQuotedInt "opcredit"


pGameState :: GenParser Char st GameState
pGameState
    = do g <- pGameMap
         char ','
         p <- pOwnId
         char ','
         n <- pPcount
         char ','
         uc <- pUnclaimed
         char ','
         mc <- pMyclaimed
         char ','
         uo <- pUnOpted
         char ','
         mo <- pMyOpted
         char ','
         oc  <- pOptionCredit
         char ','
         r  <- pRemaining
         return (GameState g p n uc mc uo mo oc r)
