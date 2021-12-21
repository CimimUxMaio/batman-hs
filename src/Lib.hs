module Lib
    ( main
    ) where

import Web.Scotty (scotty)
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
import Persistence.Database (Database, addGroup)
import Model.Group ( Group(TelegramChat, chatId) )


main :: IO ()
main = do
    config <- getConfig'
    dbRef <- newIORef $ addGroup (TelegramChat { chatId = -689220770 }) Database.init
    startBatmanJob config dbRef
    server config dbRef



startBatmanJob :: Config -> IORef Database -> IO ()
startBatmanJob config dbRef = do 
    putStrLn description'
    execSchedule $ addJob (Batman.run config dbRef) cronExp
    pure ()

    where batmanConfig = Config.batman config
          cronExp = Config.cronExp batmanConfig
          description' = case parseCronSchedule cronExp of
            Left err -> error err
            Right cs -> "running analysis " ++ describe defaultOpts cs



server :: Config -> IORef Database -> IO ()
server config dbRef = do
    let port = Config.port . Config.server $ config
    scotty port $ do
        Telegram.controller (Config.telegram config) dbRef