module Auxiliary
    ( lowcase
    , rightcase
    , injectGameState
    , reparenMove
    , takeWhileM
    , takeCountM
    , protoRead
    , protoWrite
    , debugWrite
    )
where

import Prelude hiding (tail, reverse)
import Data.Text hiding (toLower, drop)
import qualified Data.Char as DC
import Control.Monad.State
import System.IO


toLower :: String -> String
toLower = Prelude.map DC.toLower 
    
lowcase :: String -> String
lowcase = toLower
                
rightcase :: String -> String
rightcase s
    = unpack s6
    where
      s0 = pack s
      s1 = replace (pack "move\"")    (pack "Move\"")    s0
      s2 = replace (pack "stop\"")    (pack "Stop\"")    s1
      s3 = replace (pack "claim")     (pack "Claim")     s2
      s4 = replace (pack "unClaimed") (pack "unclaimed") s3
      s5 = replace (pack "myClaimed") (pack "myclaimed") s4
      s6 = replace (pack "pass")      (pack "Pass")      s5
      
injectGameState :: String -> String
injectGameState
    = unpack . (replace (pack "\"state\":{}") (pack "\"state\":{\"gamemap\":{\"sites\":[],\"rivers\":[],\"mines\":[]},\"ownid\":0,\"pcount\":0,\"unclaimed\":[],\"myclaimed\":[],\"unopted\":[],\"myopted\":[],\"opcredit\":0},\"remaining\":0}")) . pack


reparenMove :: String -> String
reparenMove
    =   unpack
      . ((flip snoc) '}')
      . (replace (pack "},{\"gamemap\":") (pack ",\"state\":{\"gamemap\":"))
      . Data.Text.reverse . Data.Text.tail . Data.Text.reverse . Data.Text.tail
      . pack



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


protoWrite :: String -> IO ()
protoWrite s
    = do hPutStr stdout s
         hFlush stdout

protoRead :: IO String
protoRead
    = do x <- takeWhileM (/= ':') (repeat getChar)
         let x' = ((read x) :: Int)
         x <- takeCountM x' (repeat getChar)
         return x
       
debugWrite :: String -> IO ()
debugWrite s
    = do hPutStrLn  stderr s
         hFlush stderr

