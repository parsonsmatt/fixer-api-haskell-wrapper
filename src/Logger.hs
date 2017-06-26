module Logger
  ( errorMsg
  , errorMsgIO
  , infoMsg
  , infoMsgIO
  , warningMsg
  , warningMsgIO
  , debugMsg
  , debugMsgIO
  ) where

import           Data.Time.LocalTime (getZonedTime)

-- This API isn't very friendly. Typically, I want a logger that looks more
-- like:
--
--     log :: MonadIO m => LogLevel -> Text -> m ()
--
-- which would let me use `log` without having to call `lift` or anything
-- like that. Other operators could be built on that, like
--
--     logError = log ErrorLevel
--
-- This is essentially what `monad-logger` does.
--
-- Alternatively, Katip lets you log arbitrary values, as long as they're
-- instances of relevant type classes. That allows you to log structured
-- and detailed logs, and potentially send them to eg Elastic Search so you
-- can easily filter and query them.

--    For the log type, prefer a concrete type, rather than a String
msg :: String -> String -> String
msg type_ message = "[" ++ type_ ++ "]: " ++ message ++ "\n"

-- add timestamp here
msgIO :: String -> IO String
msgIO message = getZonedTime >>= \y -> return $ "[" ++ (show y) ++ "]" ++ message
                                                   -- redundant paren

errorMsg :: String -> String
errorMsg = msg "ERROR"

errorMsgIO :: String -> IO String
errorMsgIO = msgIO . errorMsg

infoMsg :: String -> String
infoMsg = msg "INFO"

infoMsgIO :: String -> IO String
infoMsgIO = msgIO . infoMsg

warningMsg :: String -> String
warningMsg = msg "WARNING"

warningMsgIO :: String -> IO String
warningMsgIO = msgIO . warningMsg

debugMsg :: String -> String
debugMsg = msg "DEBUG"

debugMsgIO :: String -> IO String
debugMsgIO = msgIO . debugMsg
