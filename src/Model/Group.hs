module Model.Group where

import Model.Analysis.Analysis (ResultMap, AnalysisResult (reason, analysis, suggestion))
import Data.List.Extra ( intercalate )
import Telegram.API (sendMessage)
import Config (TelegramConfig, Config (telegram))
import Network.HTTP.Req (HttpException)
import Control.Monad.Trans.Except (ExceptT)
import Data.Aeson (ToJSON)
import GHC.Generics (Generic)
import Database.SQLite.Simple (FromRow (fromRow), field, Only (Only))
import Data.Int (Int64)
import Data.Text (Text)
import Database.SQLite.Simple.ToRow (ToRow(toRow))


newtype Group = TelegramChat { chatId :: Int }
                deriving (Show, Eq, Ord)

instance FromRow Group where
    fromRow = TelegramChat <$> field

instance ToRow Group where
    toRow (TelegramChat chatId) = toRow ("telegram" :: Text, chatId)



notify :: Config -> ResultMap -> Group -> ExceptT HttpException IO ()
notify config results group = case group of
    TelegramChat chatId -> notifyTelegramChat (Config.telegram config) chatId results



notifyTelegramChat :: TelegramConfig -> Int -> ResultMap -> ExceptT HttpException IO ()
notifyTelegramChat config chatId = sendMessage config chatId . toMessage
    where toMessage resultMap = "Batman Analysis:\n\n" ++ (intercalate "\n\n" . map formatItem $ resultMap)
          formatItem (s, results) = s ++ ":\n- " ++ (intercalate "\n- " . map formatResult $ results)
          formatResult result = analysis result ++ ": " ++ reason result ++ " -> " ++ show (suggestion result)