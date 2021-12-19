module Model.Batman where

import Config (BatmanConfig (cryptos))
import Model.Asset (getAsset, Asset)
import Model.Analysis.Analysis
    ( macd, rsi, Analysis, AnalysisResult (AnalysisResult, reason, analysis, suggestion), candlesticks )
import Data.Bifunctor (second)
import Data.List.Extra (groupOn, intercalate)


run :: BatmanConfig -> IO ()
run config = do
    putStrLn "running batman..."
    assets <- mapM (getAsset config) symbols
    let analysisList = [rsi (30, 70), macd, candlesticks]
    let results = map (second (analyse analysisList)) assets

    putStrLn $ toMessage results

    where symbols = Config.cryptos config


analyse :: [Analysis] -> Asset -> [AnalysisResult]
analyse analysisList asset = map ($ asset) analysisList



toMessage :: [(String, [AnalysisResult])] -> String
toMessage groups = "Batman Analysis:\n\n" ++ (intercalate "\n\n" . map formatGroup $ groups)
    where formatGroup (s, results) = s ++ ":\n- " ++ (intercalate "\n- " . map formatResult $ results)
          formatResult result = analysis result ++ ": " ++ reason result ++ " -> " ++ show (suggestion result)
