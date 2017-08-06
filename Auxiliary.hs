module Auxiliary
    ( lowcase
    , rightcase
    , injectMyState
    , reparenMove
    )
where

import Prelude hiding (tail, reverse)
import Data.Text hiding (toLower, drop)
import qualified Data.Char as DC


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
      s5 = replace (pack "myrivers")  (pack "myRivers")  s4
      s6 = replace (pack "pass")      (pack "Pass")      s5
      
injectMyState :: String -> String
injectMyState
    = unpack . (replace (pack "\"state\":{}") (pack "\"state\":{\"gamemap\":{\"sites\":[],\"rivers\":[],\"mines\":[]},\"ownid\":0,\"pcount\":0,\"unclaimed\":[],\"myRivers\":[],\"remaining\":0}")) . pack


reparenMove :: String -> String
reparenMove
    =   unpack
      . ((flip snoc) '}')
      . (replace (pack "},{\"gamemap\":") (pack ",\"state\":{\"gamemap\":"))
      . Data.Text.reverse . Data.Text.tail . Data.Text.reverse . Data.Text.tail
      . pack
