{-# LANGUAGE DeriveDataTypeable #-}

module Move
    ( Move(..)
    , SimpleMove(..)
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

type PunterId = Int

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
    show (Move sms _)
        = "[" ++ (foldr (\n r -> (show n) ++ ", " ++ r) "]" sms)

    show (Stop sms scs)
        =    "Game Stopped\n\n"
          ++ "Last moves:" ++ "[" ++ (foldr (\n r -> (show n) ++ ", " ++ r) "]\n" sms)
          ++ "Scores:    " ++ "(" ++ (foldr (\n r -> (show n) ++ ", " ++ r) ")" scs)

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
    deriving (Data, Typeable)

instance Show SimpleMove where
    show (Claim p s t)
        = (show p) ++ ": " ++
          (show s) ++ "--" ++ (show t)

    show (Pass p)
        = (show p) ++ ": Pass"



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
         <|> ( do char '{'
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
         <|> ( do char '{'
                  string "\"pass\":"
                  char '{'
                  p <- pPunter
                  char '}'
                  char '}'
                  return (Pass p)
             )

               
pQuotedInt :: String -> GenParser Char st Int
pQuotedInt s
    = do string $ "\"" ++ s ++ "\":"
         ds <- many digit
         return (read ds)


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
pMines
    = do string "\"mines\":"
         char '['
         mines  <- sepBy (many digit) (char ',')
         char ']'
         return $ map read $ mines
           
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


pMyRivers :: GenParser Char st [River]
pMyRivers 
    = do string "\"myrivers\":"
         char '['
         rivers  <- sepBy pRiver (char ',')
         char ']'
         return rivers


pRemaining :: GenParser Char st Int
pRemaining = pQuotedInt "remaining"


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
         mr <- pMyRivers
         char ','
         r  <- pRemaining
         return (GameState g p n uc mr r)
