module Model.Batman where

import Config (BatmanConfig (cryptos), Config (batman))
import Model.Asset (Asset, getAsset)
import Model.Analysis.Analysis
    ( macd, rsi, Analysis, AnalysisResult (AnalysisResult, reason, analysis, suggestion), candlesticks, ResultMap )
import Data.Bifunctor (second)
import Model.Group (notify, Group)
import Data.IORef (IORef, readIORef)
import Control.Exception (try, catch)
import Data.Functor ((<&>))
import Control.Monad.Trans.Except (runExceptT)
import Logging (withLogger)
import qualified Logging
import Persistence.Database (getGroups)


run :: Config -> IO ()
run config = withLogger $ \logger -> do
    Logging.info logger ["batman:run"] "running analysis"

    assets <- mapM (getAsset batmanConfig) symbols

    let analysisList = [rsi (30, 70), macd, candlesticks]
    let results = map (second (analyse analysisList)) assets

    groups <- getGroups
    mapM_ (sendResults config results) groups

    where symbols = Config.cryptos batmanConfig
          batmanConfig = Config.batman config


analyse :: [Analysis] -> Asset -> [AnalysisResult]
analyse analysisList asset = map ($ asset) analysisList


sendResults :: Config -> ResultMap -> Group -> IO ()
sendResults config results group = withLogger $ \logger -> do
    res <- runExceptT $ notify config results group
    case res of
        Left e  -> Logging.error logger ["batman:sendResults"] $ show e
        Right _ -> pure ()
    