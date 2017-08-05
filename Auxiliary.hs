module Auxiliary
    ( lowcase
    , rightcase
    )
where

import Data.Text hiding (toLower, reverse, tail, drop)
import qualified Data.Char as DC


toLower :: String -> String
toLower = Prelude.map DC.toLower 
    
lowcase :: String -> String
lowcase = toLower
                
rightcase :: String -> String
rightcase s
    = s'
    -- = if (s'!!2 == 'M')
    --   then "{\"m" ++ (drop 3 s')
    --   else s'
    where
      s0 = pack s
      s1 = replace (pack "move\"") (pack "Move\"") s0
      s2 = replace (pack "claim")  (pack "Claim")  s1
      s3 = replace (pack "pass")   (pack "Pass")   s2
      s' = unpack s3
      
