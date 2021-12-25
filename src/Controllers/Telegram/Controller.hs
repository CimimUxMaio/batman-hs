module Controllers.Telegram.Controller
    ( controller ) where

import Controllers.Telegram.Update
    ( Update(message),
      Message(NewGroup, TextMessage, NewChatMembers, LeftChat, chat), Chat (chatId), User )
import Config ( TelegramConfig(token) )
import Data.IORef ( IORef, readIORef, modifyIORef )
import Persistence.Database ( Database, addGroup )
import Web.Scotty
    ( capture, jsonData, liftAndCatchIO, post, ActionM, ScottyM )
import Controllers.Telegram.Commands
    ( Command,
      CommandResponse(ResponseMessage, NoResponse),
      CommandArgs, parseCommand )
import Telegram ( sendMessage )
import Data.Functor ((<&>))
import Model.Group (Group(TelegramChat))
import Network.HTTP.Req (HttpException)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Control.Monad.IO.Class (liftIO)
import Logging (withLogger, LoggingFunctionBuilder, prependSections)
import qualified Logging
import System.Log.FastLogger (TimedFastLogger)
import Controllers.Telegram.LogHelper (logUpdate)



controller :: TelegramConfig -> IORef Database -> ScottyM ()
controller config dbRef = do
    let token = Config.token config
    let webHook = '/' : token
    post (capture webHook) $ do
        body <- (jsonData :: ActionM Update)
        liftIO $ withLogger $ \logger -> do
            let msg = message body
            saveChat $ chat msg
            res <- runExceptT $ getAction msg config dbRef
            case res of 
                Left e  -> logUpdate Logging.error logger [] $ show e
                Right _ -> pure ()
    
    where saveChat chat = modifyIORef dbRef $ addGroup (TelegramChat . chatId $ chat)



{- HANDLERS -}

type MessageAction = TelegramConfig -> IORef Database -> ExceptT HttpException IO ()

getAction :: Message -> MessageAction
getAction (TextMessage chat msgText) config dbRef = do
    let action = parseCommand msgText <&> uncurry ($)
    case action of
        Nothing    -> pure ()
        Just doAction -> do
            response <- liftIO doAction
            handleResponse (chatId chat) response
    where handleResponse _ NoResponse = pure ()
          handleResponse chatId (ResponseMessage msg) = sendMessage config chatId msg

getAction (NewChatMembers chat users) config dbRef = liftIO . withLogger $ \logger -> do
    logUpdate Logging.debug logger ["NewChatMembers"] $ "added to chat" ++ show users
    
getAction (LeftChat chat user) config dbRef = liftIO . withLogger $ \logger -> do
    logUpdate Logging.debug logger ["LeftChat"] $ "removed from chat" ++ show user

getAction (NewGroup chat created) config dbRef = liftIO . withLogger $ \logger -> do
    logUpdate Logging.debug logger ["NewGroup"] $ "removed from chat" ++ show created