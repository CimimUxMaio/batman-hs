{-# LANGUAGE DataKinds #-}
module Telegram.API where

import Network.HTTP.Req
    ( (/:),
      (=:),
      defaultHttpConfig,
      http,
      https,
      ignoreResponse,
      jsonResponse,
      port,
      req,
      responseBody,
      runReq,
      GET(GET),
      NoReqBody(NoReqBody),
      POST(POST),
      ReqBodyJson(ReqBodyJson), Req, Scheme (Https), Url, HttpException )
import Data.Aeson ( object, KeyValue((.=)) )
import Config (getConfig, TelegramConfig, BatmanConfig (BatmanConfig))
import qualified Config
import Data.Text (pack, Text)
import Control.Monad.Trans.Except (ExceptT (ExceptT))
import Control.Exception (try, Exception)
import Model.Asset (Asset)
import Telegram.Types (Bot)



runSafeReq :: Exception e => Req a -> ExceptT e IO a
runSafeReq =  ExceptT . try . runReq defaultHttpConfig


runUnsafeReq :: Req a -> IO a
runUnsafeReq = runReq defaultHttpConfig


botUrl :: TelegramConfig -> Text -> Url 'Https
botUrl config method = https apiUrl /: pack ("bot" ++ token) /: method
    where token = Config.token config
          apiUrl = Config.apiUrl config


sendMessageReq :: TelegramConfig -> Int -> String -> Req ()
sendMessageReq config chatId text = do
    let payload = object [ "chat_id" .= chatId, "text" .= text ]
    req POST url (ReqBodyJson payload) ignoreResponse mempty 
    pure ()

    where url = botUrl config "sendMessage"


sendMessage :: TelegramConfig -> Int -> String -> ExceptT HttpException IO ()
sendMessage config chatId = runSafeReq . sendMessageReq config chatId


getBotReq :: TelegramConfig -> Req Bot 
getBotReq config = do
    r <- req GET url NoReqBody jsonResponse mempty
    pure (responseBody r)

    where url = botUrl config "getMe"


getBot :: TelegramConfig -> ExceptT HttpException IO Bot
getBot = runSafeReq . getBotReq