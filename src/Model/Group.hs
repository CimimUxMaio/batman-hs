module Model.Group where

import Model.Analysis.Analysis (ResultMap, AnalysisResult (reason, analysis, suggestion))
import Data.List.Extra ( intercalate )
import Telegram (sendMessage)
import Config (TelegramConfig, Config (telegram))


newtype Group = TelegramChat { chatId :: Int } deriving (Show, Eq, Ord)


notify :: Config -> ResultMap -> Group -> IO ()
notify config results group = case group of 
    TelegramChat chatId -> telegramChat (Config.telegram config) chatId results
    



telegramChat :: TelegramConfig -> Int -> ResultMap -> IO ()
telegramChat config chatId = sendMessage config chatId . toMessage
    where toMessage resultMap = "Batman Analysis:\n\n" ++ (intercalate "\n\n" . map formatItem $ resultMap)
          formatItem (s, results) = s ++ ":\n- " ++ (intercalate "\n- " . map formatResult $ results)
          formatResult result = analysis result ++ ": " ++ reason result ++ " -> " ++ show (suggestion result)