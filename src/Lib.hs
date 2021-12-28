module Lib
    ( main
    ) where

import Web.Scotty (scotty, Options (Options, settings), scottyOpts, scottySocket)
import qualified Controllers.Telegram.Controller as Telegram
import GHC.Generics (Generic)
import Data.Aeson ( FromJSON, decode )
import Data.ByteString.Lazy.Char8 (pack)
import qualified Config
import Config (Config, getConfig', BatmanConfig)
import System.Cron (execSchedule, addJob, parseCronSchedule, describe)
import Data.Text (Text)
import System.Cron.Describe (defaultOpts)
import qualified Model.Batman as Batman
import qualified Persistence.Database as Database
import Data.IORef (newIORef, IORef)
import Model.Group ( Group(TelegramChat, chatId) )
import Logging (withLogger, scottyLogging)
import qualified Logging
import Data.List.Extra (lower)
import Data.Default (def)
import Network.Wai.Handler.Warp.Internal ( Settings(settingsPort), settingsLogger )
import Network.Wai.Handler.Warp (defaultSettings)
import System.Log.FastLogger (TimedFastLogger)
import Network.Wai (Request (httpVersion, requestMethod, rawPathInfo, rawQueryString))
import Network.HTTP.Types (Status (statusMessage, statusCode))


main :: IO ()
main = do
    config <- getConfig'
    scheduleBatman config
    server config


scheduleBatman :: Config -> IO ()
scheduleBatman config = withLogger $ \logger -> do
    execSchedule $ addJob (Batman.run config) cronExp
    Logging.info logger [location "scheduleBatman"] description'

    where batmanConfig = Config.batman config
          cronExp = Config.cronExp batmanConfig
          description' = case parseCronSchedule cronExp of
            Left err -> error err
            Right cs -> "batman scheduled for " ++ lower (describe defaultOpts cs)


server :: Config -> IO ()
server config = withLogger $ \logger -> do
    let port = Config.port . Config.server $ config
    Logging.info logger [location "server"] $ "server started on port " ++ show port
    scottyOpts (opts port logger) $ do
        Telegram.controller (Config.telegram config)

    where opts port logger = Options 0 $ defaultSettings { settingsPort = port, settingsLogger = scottyLogging logger }


location :: String -> String
location funcName = "main:" ++ funcName