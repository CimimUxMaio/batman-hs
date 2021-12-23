module Telegram where

import Network.HTTP.Req
    ( defaultHttpConfig,
      (/:),
      https,
      ignoreResponse,
      req,
      runReq,
      POST(POST),
      ReqBodyJson(ReqBodyJson), HttpException)
import Data.Aeson ( object, KeyValue((.=)) )
import Config (getConfig, TelegramConfig)
import qualified Config
import Data.Text (pack)
import Control.Monad.Trans.Except (ExceptT (ExceptT))
import Control.Exception (try, Exception)



sendMessage :: Exception e => TelegramConfig -> Int -> String -> ExceptT e IO ()
sendMessage config chatId text = ExceptT . try $ request
    where token = Config.token config
          apiUrl = Config.apiUrl config
          url = https apiUrl /: pack ("bot" ++ token) /: "sendMessage"
          request = runReq defaultHttpConfig $ do
              let payload = object [ "chat_id" .= chatId, "text" .= text ]
              req POST url (ReqBodyJson payload) ignoreResponse mempty 
              pure ()