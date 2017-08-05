{-# LANGUAGE DeriveDataTypeable #-}

module Main
where

import Prelude hiding (map)

import Text.JSON.Generic
import Control.Monad.State
import Network
import System.IO
import System.Environment

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
    = do args <- getArgs
         let port = (read . head $ args) :: PortNumber

         putStrLn $ "Connecting to: " ++ serverAddress ++ (':' : show port) ++ "\n"
                  
         h <- connectTo serverAddress (PortNumber port)
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
         hPutStr h (pickle . lowcase . encodeJSON $ Ready p)
         putStrLn $ "Your are Punter #" ++ (show p) ++ "\n"

         let doOwnMove s =
               do (m, s) <- runStateT nextMove s
                  putStrLn $ "   " ++ show m
                  hPutStr h (pickle . lowcase . encodeJSON $ m)
                  return s

         let loop s = 
               do putStrLn $ (show . remaining $ s) ++ " remaining Moves"

                  l <- hGetLine h >>= (\x -> return $ unpickle x)
                  let lastServerMove = ((decodeJSON . rightcase $ l) :: M.Move)
                      lastMoves      = M.moves lastServerMove
                  putStrLn $ show lastServerMove

                  (m, s) <- runStateT (foldM (\_ n -> eliminateMove n) (M.Pass p) lastMoves) s

                  let remainingMoves = remaining s

                  
                  when (not(M.isStopped lastServerMove)) (doOwnMove s >>= (\x -> loop x))
         loop s
         return ()
