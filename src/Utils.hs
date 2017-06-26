module Utils
  ( parseFixerDate
  ) where

import Text.Parsec
import Text.Parsec.Char (digit, char, oneOf)
import Text.Parsec.Combinator (count)
import Types (FixerDate(..))

-- Data.Time has a parsing module that you can use in this case.
--
-- https://hackage.haskell.org/package/time-1.8.0.2/docs/Data-Time-Format.html
--
-- parseTimeM True "%Y-%M-%D" :: Either String Day
parseFixerDate :: String -> Either ParseError FixerDate
parseFixerDate x = runParser pp "" "" x
  where
    pp =
    -- redundant paren
      (do year1 <- (oneOf "12") -- redundant paren
          year2 <- (count 3 digit)
          _ <- char '-'
          month1 <- (oneOf "01")
          month2 <- digit
          _ <- char '-'
          day1 <- (oneOf "0123")
          day2 <- digit
          -- read is unsafe. you're writing a parser
          -- combinator. Why not use the digit facilities?
          let year = (read $ year1 : year2) :: Int
              month = (read $ month1 : month2 : []) :: Int
              day = (read $ day1 : day2 : []) :: Int
          return $ FixerDate year month day)

-- Rather than reading strings and then calling `read` to make them into
-- ints, write a function that parses a digit and then checks that it's
-- valid:

digitIn :: [Int] -> Parser Int
digitIn vals = do
  x <- digit
  if x `elem` vals 
    then pure x
    else fail $ "Expected a digit in " ++ show vals ++ ", but " ++ show x ++ "is not in that set."

-- now the above function can be rewritten safely.
