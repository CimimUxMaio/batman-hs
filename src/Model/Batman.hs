module Model.Batman where

import Config (BatmanConfig (cryptos))
import Model.Asset (getAsset, Asset)
import Model.Analysis.Analysis
import Data.Bifunctor (second)


run :: BatmanConfig -> IO ()
run config = do
    putStrLn "running batman..."
    assets <- mapM (getAsset config) symbols
    let analysisList = [rsi, macd]
    mapM_ (print . second (analyse analysisList)) assets

    where symbols = Config.cryptos config



analyse :: [Analysis] -> Asset -> [AnalysisResult]
analyse analysisList asset = map ($ asset) analysisList