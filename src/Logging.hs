module Logging where

import System.Log.FastLogger
    ( TimeFormat
    , newTimeCache
    , newTimedFastLogger
    , LogType' (LogStdout)
    , simpleTimeFormat
    , defaultBufSize
    , TimedFastLogger, ToLogStr (toLogStr), LogStr )
import Prelude hiding (error, log)
import Control.Exception (bracket)
import Network.HTTP.Types (Status (statusCode, statusMessage))
import Network.Wai (Request (httpVersion, requestMethod, rawPathInfo, rawQueryString))
import Data.List.Extra (mconcatMap)
import Data.ByteString.Char8 (unpack)


timeFormat :: TimeFormat
timeFormat = "%d/%b/%Y %T %z"


getLogger :: IO (TimedFastLogger, IO ())
getLogger = do
    timeCache <- newTimeCache timeFormat
    newTimedFastLogger timeCache (LogStdout defaultBufSize)

withLogger :: (TimedFastLogger -> IO a) -> IO a
withLogger computation = bracket getLogger snd $ \(logger, _) -> computation logger


formatLog :: [String] -> String -> LogStr
formatLog sections msg = mconcatMap section sections <> " " <> toLogStr msg <> "\n"
    where section str = toLogStr ("[" ++ str ++ "]")


type LoggingFunction = [String] -> String -> IO ()
type LoggingFunctionBuilder = TimedFastLogger -> LoggingFunction
type LogLevel = String
 

log :: LogLevel -> LoggingFunctionBuilder
log level logger sections msg = logger $ \time ->
    formatLog (unpack time : level : sections) msg


info :: LoggingFunctionBuilder
info = log "INFO"

debug :: LoggingFunctionBuilder
debug = log "DEBUG"

warn :: LoggingFunctionBuilder
warn = log "WARN"

error :: LoggingFunctionBuilder
error = log "ERROR"


prependSections :: [String] -> LoggingFunctionBuilder -> LoggingFunctionBuilder
prependSections newSections logBuilder logger sections = logBuilder logger (newSections ++ sections)


scottyLogging :: TimedFastLogger -> Request -> Status -> Maybe Integer -> IO ()
scottyLogging logger request status _ =
    info logger ["API"] $ requestDesc ++ " - " ++ statusDesc

    where wrap s str = s : str ++ [s]
          statusDesc = show (statusCode status) ++ " " ++ show (statusMessage status)
          requestDesc = unwords [ show (httpVersion request), requestUrlDesc ]
          requestUrlDesc = wrap '"' . unwords . map (filter (/= '"') . show) $ [ requestMethod request
                                                                               , rawPathInfo request
                                                                               , rawQueryString request ]