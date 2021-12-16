module Lib
    ( main
    ) where

import Web.Scotty (scotty)
import qualified Controllers.Telegram as Telegram
import GHC.Generics (Generic)
import Data.Aeson ( FromJSON, decode )
import Data.ByteString.Lazy.Char8 (pack)
import qualified Config
import Config (Config, getConfig')
import System.Cron (execSchedule, addJob, parseCronSchedule, describe)
import Data.Text (Text)
import System.Cron.Describe (defaultOpts)


main :: IO ()
main = do
    config <- getConfig'
    startAnalysisJob "* * * * *"
    server config



startAnalysisJob :: Text -> IO ()
startAnalysisJob cronExp = do 
    putStrLn description'
    execSchedule $ addJob runAnalysis cronExp
    pure ()

    where description' = case parseCronSchedule cronExp of
            Left err -> error err
            Right cs -> "running analysis " ++ describe defaultOpts cs



server :: Config -> IO ()
server config = do
    let port = Config.port . Config.server $ config
    scotty port $ do
        Telegram.controller (Config.telegram config)



runAnalysis :: IO ()
runAnalysis = do
    putStrLn "Running analysis..."
    