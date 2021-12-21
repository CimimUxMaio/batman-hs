module Telegram where

import Network.HTTP.Req
    ( defaultHttpConfig,
      (/:),
      https,
      ignoreResponse,
      req,
      runReq,
      POST(POST),
      ReqBodyJson(ReqBodyJson) )
import Data.Aeson ( object, KeyValue((.=)) )
import Config (getConfig, TelegramConfig)
import qualified Config
import Data.Text (pack)


sendMessage :: TelegramConfig -> Int -> String -> IO ()
sendMessage config chatId text = runReq defaultHttpConfig $ do
    let payload = object [ "chat_id" .= chatId, "text" .= text ]
    req POST url (ReqBodyJson payload) ignoreResponse mempty
    pure ()

    where token = Config.token config
          apiUrl = Config.apiUrl config
          url = https apiUrl /: pack ("bot" ++ token) /: "sendMessage"