{-# LANGUAGE OverloadedStrings #-}

module Client
  ( getFixerUrl
  ) where

-- Our handy module.
import Network.Wreq

import Control.Exception (IOException)

-- Operators such as (&) and (.~).
import Control.Lens
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either
import Control.Monad.Trans.Writer
import Data.Aeson
import Data.ByteString.Lazy
import Network.HTTP.Client (HttpException)
import Types

import Logger

{- 
this type signature gives me some pause:
1. `EitherT` is deprecated; prefer ExceptT
2. `WriterT` has space leak problems that you should be aware of: https://twitter.com/gabrielg439/status/659170544038707201
3. `String` is inefficient; prefer `Text` for textual stuff and `ByteString` for raw bytes
4. Your "app monad" should be defined as a type synonym or a newtype around your stack, so 
   a change to the app won't have to made in all of the functions that use it.

It appears that you're using `WriterT` for logging. I would suggest that you drop
that and instead use something like Katip or MonadLogger. MonadLogger is a bit simpler,
and Katip has a really nice interface for structured logging.
-}
getBasic :: String -> EitherT HttpException (WriterT String IO) ByteString
getBasic url = do
  -- The pattern `liftIO (infoMsgIO str) >>= lift . tell` is common thoughought
  -- this application, so you probably want to define a helper for it.
  msg <- liftIO . infoMsgIO $ " Sent GET request to " ++ (show url)
  lift . tell $ msg
  -- This is a redundant parentheses. I would also suggest that you put
  -- IO exception catching code as close as possible to the code that
  -- might throw the exception. The block you're catching exceptions in
  -- does two things: HTTP request and log the response. I'd do:
  -- 
  --    resp <- liftIO (view responseBody <$> get url)
  --                `catch` \e ->
  --                    logErrorIO (displayException e) >> left e
  --    logInfoIO $ "Server sent back " <> show resp
  --    right resp
  (catch
                            -- (view responseBody) also works here, or
                            -- (^. responseBody) if you're a fan of operator sections
     (do resp <- liftIO $ fmap (\x -> x ^. responseBody) . get $ url
         msg <- liftIO $ infoMsgIO $ " Server sent back " ++ (show resp)
         lift . tell $ msg
         right resp)
     (\exception -> do
        msg <- liftIO $ errorMsgIO . displayException $ exception
        lift . tell $ msg
        left exception))

-- decoding error possibility
                              -- There is no point to having a String-ly
                              -- typed exception. Strongly prefer real
                              -- exception types that actually map to 
                              -- problems you encounter. Otherwise you can't
                              -- easily catch them, and then what's the point?
getFixerUrl :: String -> EitherT String (WriterT String IO) FixerResponse
getFixerUrl url =
  -- I think I would prefer to see `do` notation here.
  getBasic url & bimapEitherT displayException id >>=
  (\x ->
       -- redundant paren
     case (eitherDecode x) of
       Left y ->
         -- redundant paren
         (do msg <- liftIO $ errorMsgIO y
             lift . tell $ msg
             left y)
       Right z ->
         -- redundant paren
         (do msg <- liftIO $ infoMsgIO $ show z
             lift . tell $ msg
             right z))
