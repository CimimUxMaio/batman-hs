module TelegramChat where

import Network.HTTP.Req
import Control.Monad.IO.Class
import Data.Aeson
import Config (getConfig, TelegramConfig)
import qualified Config
import Data.Text (pack)

sendMessage :: TelegramConfig -> Integer -> String -> IO ()
sendMessage config chatId text = runReq defaultHttpConfig $ do
    let payload = object [ "chat_id" .= chatId, "text" .= text ]
    req
        POST
        (https (pack apiUrl) /: pack ("bot" ++ token) /: "sendMessage")
        (ReqBodyJson payload)
        ignoreResponse
        mempty
    pure ()

    where token = Config.token config
          apiUrl = Config.apiUrl config