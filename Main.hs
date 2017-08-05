{-# LANGUAGE DeriveDataTypeable #-}

module Main
where

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


main :: IO ()
main
    = do h <- connectTo "punter.inf.ed.ac.uk" (PortNumber 9007)
         hPutStr h (pickle . lowcase . encodeJSON $ Name "frosch03")
         _ <- hGetLine h

         l <- hGetLine h >>= (\x -> return $ unpickle x)
         putStrLn ("GameState received")
         putStrLn (show l)

         let s = initialize l
             p = punter . setup $ s

         putStrLn ("Punter ID:" ++ show p)
         hPutStr h (pickle . lowcase . encodeJSON $ Ready p)

         let loop s = 
               do putStrLn $ (show . remaining $ s) ++ " remaining Moves"
                  l <- hGetLine h >>= (\x -> return $ unpickle x)
                  putStrLn "Opponent moved:"
                  putStrLn $ show $ ((decodeJSON . rightcase $ l) :: M.Move)
                  let lastMove = head . M.moves $ ((decodeJSON . rightcase $ l) :: M.Move)
                  (_, s) <- runStateT (eliminateMove lastMove) s

                  let remainingMoves = remaining s
                  putStrLn $ (show remainingMoves) ++ " remaining Moves"

                  (m, s) <- runStateT nextMove s
                  putStrLn "Own move:"
                  putStrLn $ show m
                  hPutStr h (pickle . lowcase . encodeJSON $ m)
                  putStrLn "---loop---"
                  when (remaining s > 0) (loop s)
         loop s

         l <- hGetLine h >>= (\x -> return $ unpickle x)
         putStrLn (show l)

         return ()


