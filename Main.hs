{-# LANGUAGE DeriveDataTypeable #-}

module Main
where

import Prelude hiding (map)

import Text.JSON.Generic
import Control.Monad.State
import Network
import System.IO
import System.Environment
import qualified Data.Text as T

import qualified Move as M
import Map 
import Auxiliary 
import Punter
import Protocol
import Game


serverAddress = "punter.inf.ed.ac.uk"
player = Name "frosch03"


main :: IO ()
main
    = do hSetBuffering stdin  NoBuffering
         hSetBuffering stdout NoBuffering
         hSetBuffering stderr NoBuffering

         protoWrite (pickle . lowcase . encodeJSON $ player)

         _ <- protoRead

         l <- protoRead

         let doReady l
                 = do debugWrite ("GameState received")
                      let s   = initialize l
                          set = settings $ ((read l) :: Setup)
                          p   = ownid $ s
                          n   = pcount $ s
                          lSs = length . sites  . gamemap $ s
                          lRs = length . rivers . gamemap $ s
                          ms  = mines  . gamemap $ s
                          exS = splurges $ set
                          exF = futures  $ set
                          exO = options  $ set

                      debugWrite $  (show n) ++ " Punters | "
                                 ++ (show lSs) ++ " Sites | "
                                 ++ (show lRs) ++ " Rivers"
                      debugWrite $ "Mines:      "  ++ (show ms) 
                      debugWrite $ "Extensions: "  ++
                                     (if exF then "F" else "_") ++ "/" ++
                                     (if exS then "S" else "_") ++ "/" ++
                                     (if exO then "O" else "_")
                      protoWrite (pickle . lowcase . encodeJSON $ Ready p s)
                      debugWrite $ "Your are Punter #" ++ (show p) ++ "\n"

         let doOwnMove s =
                 do (sm, s) <- runStateT nextMove s
                    debugWrite $ "Me: " ++ (drop 3 $ show sm)
                    protoWrite (pickle . lowcase . reparenMove . encodeJSON $ (sm, s))
                    return s


         let doMove l =
                 do let (M.Move lastMoves s) = ((read l) :: M.Move)
                        p = ownid $ s
                    debugWrite $ "Server: " ++ (show ((read l) :: M.Move))
                    (m, s) <- runStateT (foldM (\_ n -> eliminateMove n) (M.Pass p) lastMoves) s

                    let remainingMoves = remaining
                  
                    doOwnMove s
                    return ()

         let doStop l =
                 do debugWrite $ "Server: " ++ (show ((read l) :: M.Move))
                    return ()


         (case (tail . takeWhile (/= ':') $ l) of
            "\"punter\"" -> doReady l
            "\"move\""   -> doMove l
            "\"stop\""   -> doStop l)

         return ()
