{-# LANGUAGE DeriveDataTypeable #-}

module Parsers
where


import Text.ParserCombinators.Parsec



pQuotedInt :: String -> GenParser Char st Int
pQuotedInt s
    = do string $ "\"" ++ s ++ "\":"
         ds <- many digit
         return (read ds)

pBool :: GenParser Char st Bool
pBool
    =     (string "true"  >> return True)
      <|> (string "True"  >> return True)
      <|> (string "false" >> return False)
      <|> (string "False" >> return False)

pQuotedBool :: String -> GenParser Char st Bool
pQuotedBool s
    = do string $ "\"" ++ s ++ "\":"
         b <- pBool
         return b

-- pStringUntil :: [Char] -> GenParser Char st String
-- pStringUntil stopChars
--     = do s <- many1 $ noneOf stopChars
--          return s

pQuotedStrings :: String -> GenParser Char st [String]
pQuotedStrings s
    = do string $ "\"" ++ s ++ "\":"
         char '['
         xs  <- sepBy (many1 $ noneOf ",") (char ',')
         char ']'
         return $ xs
           
pQuotedInts :: String -> GenParser Char st [Int]
pQuotedInts s
    = do string $ "\"" ++ s ++ "\":"
         char '['
         ints  <- sepBy (many digit) (char ',')
         char ']'
         return $ map read $ ints
           
