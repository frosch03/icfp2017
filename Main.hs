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


takeWhileM :: Monad m => (a -> Bool) -> [m a] -> m [a]
takeWhileM p (ma : mas) = do
    a <- ma
    if p a
      then liftM (a :) $ takeWhileM p mas
      else return []
takeWhileM _ _ = return []
                 
       
takeCountM :: Monad m => Int -> [m a] -> m [a]
takeCountM 1 (ma : _)
    = do a <- ma
         return [a]
takeCountM cnt (ma : mas)
    = do a <- ma
         liftM (a :) $ takeCountM (cnt - 1) mas
takeCountM _ _ = return []


sWrite :: String -> IO ()
sWrite s
    = do hPutStr stdout s
         hFlush stdout

sRead :: IO String
sRead
    = do x <- takeWhileM (/= ':') (repeat getChar)
         let x' = ((read x) :: Int)
         x <- takeCountM x' (repeat getChar)
         return x
       
dWrite :: String -> IO ()
dWrite s
    = do hPutStrLn stderr s
         hFlush stderr

main :: IO ()
main
    = do hSetBuffering stdin  NoBuffering
         hSetBuffering stdout NoBuffering
         hSetBuffering stderr NoBuffering

         sWrite (pickle . lowcase . encodeJSON $ player)

         _ <- sRead

         l <- sRead
         dWrite ("GameState received")

         let s   = initialize l
             p   = punter  . setup $ s
             n   = punters . setup $ s
             lSs = length . sites  . map . setup $ s
             lRs = length . rivers . map . setup $ s
             lMs = length . mines  . map . setup $ s

         dWrite $  (show n) ++ " Punters | "
                            ++ (show lSs) ++ " Sites | "
                            ++ (show lRs) ++ " Rivers | "
                            ++ (show lMs) ++ " Mines "
         sWrite (pickle . lowcase . encodeJSON $ Ready p)
         dWrite $ "Your are Punter #" ++ (show p) ++ "\n"

         let doOwnMove s =
               do (m, s) <- runStateT nextMove s
                  dWrite $ "Me:     " ++ show m
                  sWrite (pickle . lowcase . encodeJSON $ m)
                  return s

         let loop s = 
               do l <- sRead
                  let lastServerMove = ((decodeJSON . rightcase $ l) :: M.Move)
                      lastMoves      = M.moves lastServerMove
                  dWrite $ "Server: " ++ show lastServerMove
                  (m, s) <- runStateT (foldM (\_ n -> eliminateMove n) (M.Pass p) lastMoves) s

                  let remainingMoves = remaining s

                  
                  when (not(M.isStopped lastServerMove)) (doOwnMove s >>= (\x -> loop x))
         loop s
         return ()
