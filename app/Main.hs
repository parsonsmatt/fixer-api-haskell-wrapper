{-# LANGUAGE OverloadedStrings #-}

-- This is a lot of logic to go into a Main module. I typically prefer to
-- store all of the logic inside of a library module, and then my Main
-- function is often just
--
--     module Main where
--
--     import qualified App
--
--     main :: IO ()
--     main = App.runApplication
module Main where

-- It's nice to separate import lists so that internal/library modules are
-- separate from package modules. Like:
--
-- import Control.Monad.IO.Class
-- import Control.Monad.Trans.Either
-- import Data.Either
--
-- import Client
-- import Types
-- import Util
import           Client
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Either
import           Control.Monad.Trans.Writer
import           Data.Either
import           Data.Monoid
import           Logger
import           Network.HTTP.Types.Status
import           Text.Read                  (readMaybe)
import           Types
import           Utils
import           Web.Scotty

-- don't use snake case
constant_base_url :: String
constant_base_url = "https://api.fixer.io/"

-- constant is implied by a pure value at the top level.
constant_latest_url :: String
constant_latest_url = constant_base_url ++ "latest"

-- This isn't a constant, it's a function!
constant_historical_url :: String -> String
-- This doesn't give you any way of verifying that the parameter makes any
-- sense. Instead of accepting a `String`, why not accept something that
-- you know would maek a valid historical URL?
constant_historical_url = (++) constant_base_url

constant_set_base_url :: Country -> String
constant_set_base_url x = mconcat [constant_base_url, "latest?base=", show x]

constant_convert_between_url :: Country -> Country -> String
constant_convert_between_url x y =
  mconcat [constant_base_url, "latest?symbols=", show x, ",", show y]

commonAction :: String -> ActionM ()
commonAction url =
    -- redundant paren
  (do (x, logs) <- liftIO (runWriterT . runEitherT . getFixerUrl $ url)
      liftIO (putStr logs)
      either (\x -> status status500 >> text "Please try later") json x)
           -- variable binding shadowing. if you're not going to use it,
           -- then just use _, like \_ -> ...

-- routes
getLatest :: ScottyM ()
getLatest = get "/" (commonAction constant_latest_url)

getWithDate :: ScottyM ()
getWithDate =
  get
    "/date/:date"
    (do date <- param "date"
        case (parseFixerDate date) of
          Left y ->
            (do liftIO
                  $(errorMsgIO $
                    "Date :" ++
                    date ++ " sent was/is not valid \n error msg: " ++ "{{" ++ (show y) ++ "}}") >>=
                  putStr
                status status500
                text "Invalid Date")
          Right x -> commonAction $ constant_historical_url date)

getWithBase :: ScottyM ()
getWithBase =
  get
    "/:base"
    -- imo, it'd be cleaner to have a $, but that's a style preference.
    (do base <- param $ "base"
              -- redundant paren
        case (readMaybe base :: Maybe Country) of
          Nothing ->
                       -- why is this a template haskell splice?
            (do liftIO $(errorMsgIO $ "Country Code :" ++ base ++ " sent was/is not valid") >>=
                  putStr
                status status500
                text "Check Country Code")
          Just code -> commonAction . constant_set_base_url $ code)

getConvertFromBaseTo :: ScottyM ()
getConvertFromBaseTo =
  get
    "/:base/:to"
    (do codes@[base, to] <- mapM param ["base", "to"]
        case (mapM readMaybe codes :: Maybe [Country]) of
          Nothing ->
            (do liftIO $
                  (errorMsgIO $
                   "One or both Country codes (" ++ base ++ "," ++ to ++ ") sent were/are not valid") >>=
                  putStr
                status status500
                text "Check Country Code")
          Just [c1, c2] -> commonAction $ constant_convert_between_url c1 c2)
          -- you're missing a pattern match here: what if it's Just [], or
          -- Just [a], or similar? I know you're constructing the list just
          -- above and this happens to be safe, but you're throwing away
          -- the utility of the type system.
          -- Instead, consider:
          --     (baseStr, toStr) <- (,) <$> param "base" <*> param "to"
          --     let maybeCodes = (,) <$> readMaybe baseStr <*> readMaybe toStr
          --     case maybeCodes of
          --       Nothing -> ...
          --       Just (c1, c2) -> ...

-- I typically prefer to see main near the top of the main module. That
-- makes it easier to scan the document from top to bottom and get a sense
-- of what uses what.
main :: IO ()
main = scotty 8000 (getLatest >> getWithDate >> getWithBase >> getConvertFromBaseTo)
                   -- I'm not really feeling this. It'd be easier to see do
                   -- notation I think.
