module Lib
    ( main
    ) where

import Web.Scotty (scotty)
import qualified Controllers.Telegram as Telegram
import GHC.Generics (Generic)
import Data.Aeson ( FromJSON, decode )
import Data.ByteString.Lazy.Char8 (pack)
import qualified Config
import Config (Config, getConfig', BatmanConfig)
import System.Cron (execSchedule, addJob, parseCronSchedule, describe)
import Data.Text (Text)
import System.Cron.Describe (defaultOpts)
import qualified Model.Batman as Batman


main :: IO ()
main = do
    config <- getConfig'
    startBatmanJOb $ Config.batman config
    server config



startBatmanJOb :: BatmanConfig  -> IO ()
startBatmanJOb config = do 
    putStrLn description'
    execSchedule $ addJob (Batman.run config) cronExp
    pure ()

    where cronExp = Config.cronExp config
          description' = case parseCronSchedule cronExp of
            Left err -> error err
            Right cs -> "running analysis " ++ describe defaultOpts cs



server :: Config -> IO ()
server config = do
    let port = Config.port . Config.server $ config
    scotty port $ do
        Telegram.controller (Config.telegram config)