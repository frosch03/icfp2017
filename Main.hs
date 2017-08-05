{-# LANGUAGE DeriveDataTypeable #-}

module Main
where

import Prelude hiding (map)

import Text.JSON.Generic
import Control.Monad.State
import Network
import System.IO

import qualified Move as M
import Map 
import Auxiliary 
import Punter
import Protocol
import Game


serverAddress = "punter.inf.ed.ac.uk"
serverPort    = 9006

player = Name "frosch03"

main :: IO ()
main
    = do h <- connectTo serverAddress (PortNumber serverPort)
         hPutStr h (pickle . lowcase . encodeJSON $ player)
         _ <- hGetLine h

         l <- hGetLine h >>= (\x -> return $ unpickle x)
         putStrLn ("GameState received")

         let s   = initialize l
             p   = punter  . setup $ s
             n   = punters . setup $ s
             lSs = length . sites  . map . setup $ s
             lRs = length . rivers . map . setup $ s
             lMs = length . mines  . map . setup $ s

         putStrLn $      (show n)   ++ " Punters | "
                      ++ (show lSs) ++ " Sites | "
                      ++ (show lRs) ++ " Rivers | "
                      ++ (show lMs) ++ " Mines "
         putStrLn $ "Your are Punter #" ++ (show p)

         hPutStr h (pickle . lowcase . encodeJSON $ Ready p)

         let loop s = 
               do putStrLn $ (show . remaining $ s) ++ " remaining Moves"
                  l <- hGetLine h >>= (\x -> return $ unpickle x)

                  putStrLn $ show $ ((decodeJSON . rightcase $ l) :: M.Move)
                  let lastMove = head . M.moves $ ((decodeJSON . rightcase $ l) :: M.Move)
                  (_, s) <- runStateT (eliminateMove lastMove) s

                  let remainingMoves = remaining s
                  putStrLn $ (show remainingMoves) ++ " remaining Moves"

                  (m, s) <- runStateT nextMove s

                  putStrLn $ "   " ++ show m
                  hPutStr h (pickle . lowcase . encodeJSON $ m)

                  when (remaining s > 0) (loop s)
         loop s

         l <- hGetLine h >>= (\x -> return $ unpickle x)
         putStrLn $ show ((decodeJSON . rightcase $ l) :: M.Move)

         return ()
