module Telegram where

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
      ReqBodyJson(ReqBodyJson) )
import Data.Aeson ( object, KeyValue((.=)) )
import Config (getConfig, TelegramConfig, BatmanConfig (BatmanConfig))
import qualified Config
import Data.Text (pack)
import Control.Monad.Trans.Except (ExceptT (ExceptT))
import Control.Exception (try, Exception)
import Model.Asset (Asset)



sendMessage :: Exception e => TelegramConfig -> Int -> String -> ExceptT e IO ()
sendMessage config chatId text = ExceptT . try $ request
    where token = Config.token config
          apiUrl = Config.apiUrl config
          url = https apiUrl /: pack ("bot" ++ token) /: "sendMessage"
          request = runReq defaultHttpConfig $ do
              let payload = object [ "chat_id" .= chatId, "text" .= text ]
              req POST url (ReqBodyJson payload) ignoreResponse mempty 
              pure ()



getAsset :: BatmanConfig -> String -> IO (String, Asset)
getAsset config symbol = runReq defaultHttpConfig $ do
        r <- req GET url NoReqBody jsonResponse (mconcat options)
        pure (symbol, responseBody r :: Asset)

    where apiUrl = Config.dataApiUrl config
          url = http apiUrl /: "binance" /: pack symbol
          options = [ port 8000
                    , "interval" =: Config.dataInterval config
                    , "limit" =: Config.dataAmount config ]