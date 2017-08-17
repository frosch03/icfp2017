{-# LANGUAGE DeriveDataTypeable #-}

module Parsers
where


import Text.ParserCombinators.Parsec



pQuotedInt :: String -> GenParser Char st Int
pQuotedInt s
    = do string $ "\"" ++ s ++ "\":"
         ds <- pInt
         return ds

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
           
pQuotedString :: String -> GenParser Char st String
pQuotedString s
    = do string $ "\"" ++ s ++ "\":"
         char '\"'
         xs  <- many1 $ noneOf "\""
         char '\"'
         return $ xs
           
pQuotedInts :: String -> GenParser Char st [Int]
pQuotedInts s
    = do string $ "\"" ++ s ++ "\":"
         char '['
         ints  <- sepBy (pInt) (char ',')
         char ']'
         return $ ints
           
pInt :: GenParser Char st Int
pInt
    = do try ( do (char '-')
                  ds <- many digit
                  return (-1 * (read ds))
             )
         <|> ( do ds <- many digit
                  return ((read ds))
             )
