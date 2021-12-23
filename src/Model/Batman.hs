module Model.Batman where

import Config (BatmanConfig (cryptos), Config (batman))
import Model.Asset (getAsset, Asset)
import Model.Analysis.Analysis
    ( macd, rsi, Analysis, AnalysisResult (AnalysisResult, reason, analysis, suggestion), candlesticks, ResultMap )
import Data.Bifunctor (second)
import Data.List.Extra (groupOn, intercalate)
import Persistence.Database (Database (groups))
import Model.Group (notify, Group)
import Data.IORef (IORef, readIORef)
import Control.Exception (try, catch)
import Data.Functor ((<&>))
import Control.Monad.Trans.Except (runExceptT)


run :: Config -> IORef Database -> IO ()
run config dbRef = do
    putStrLn "running batman..."

    assets <- mapM (getAsset batmanConfig) symbols

    let analysisList = [rsi (30, 70), macd, candlesticks]
    let results = map (second (analyse analysisList)) assets

    db <- readIORef dbRef
    mapM_ (sendResults config results) $ groups db

    where symbols = Config.cryptos batmanConfig
          batmanConfig = Config.batman config


analyse :: [Analysis] -> Asset -> [AnalysisResult]
analyse analysisList asset = map ($ asset) analysisList


sendResults :: Config -> ResultMap -> Group -> IO ()
sendResults config results group = do
    res <- runExceptT $ notify config results group
    case res of
        Left e  -> print e
        Right _ -> print "OK"