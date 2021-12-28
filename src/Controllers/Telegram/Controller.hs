module Controllers.Telegram.Controller
    ( controller ) where

import Telegram.Types
    ( Update(message),
      Message(NewGroup, TextMessage, NewChatMembers, LeftChat, chat), Chat (chatId), User )
import Config ( TelegramConfig(token) )
import Data.IORef ( IORef, readIORef, modifyIORef )
import Web.Scotty
    ( capture, jsonData, liftAndCatchIO, post, ActionM, ScottyM )
import Controllers.Telegram.Commands
    ( Command,
      CommandResponse(ResponseMessage, NoResponse),
      CommandArgs, parseCommand )
import Telegram.API ( sendMessage, getBot )
import Data.Functor ((<&>))
import Model.Group (Group(TelegramChat))
import Network.HTTP.Req (HttpException)
import Control.Monad.Trans.Except (ExceptT (ExceptT), runExceptT)
import Control.Monad.IO.Class (liftIO)
import Logging (withLogger, LoggingFunctionBuilder, prependSections)
import qualified Logging
import System.Log.FastLogger (TimedFastLogger)
import Controllers.Telegram.LogHelper (logUpdate)
import Control.Exception (try)
import Control.Monad (when)
import Persistence.Database (saveGroup, deleteGroup)



controller :: TelegramConfig -> ScottyM ()
controller config = do
    let token = Config.token config
    let webHook = '/' : token
    post (capture webHook) $ do
        body <- (jsonData :: ActionM Update)
        liftIO $ withLogger $ \logger -> do
            let msg = message body
            res <- runExceptT $ getAction msg config
            case res of
                Left e  -> logUpdate Logging.error logger [] $ show e
                Right _ -> pure ()


{- HANDLERS -}

type MessageAction = TelegramConfig -> ExceptT HttpException IO ()

getAction :: Message -> MessageAction
getAction (TextMessage chat msgText) config = do
    let action = parseCommand msgText <&> uncurry ($)
    case action of
        Nothing    -> pure ()
        Just doAction -> do
            response <- liftIO doAction
            handleResponse (chatId chat) response
    where handleResponse _ NoResponse = pure ()
          handleResponse chatId (ResponseMessage msg) = sendMessage config chatId msg

getAction (NewChatMembers chat users) config = ExceptT . withLogger $ \logger -> runExceptT $ do
    bot <- getBot config
    when (bot `elem` users) $ liftIO $ do
        logUpdate Logging.debug logger ["NewChatMembers"] $ "bot joined new chat: " ++ show (chatId chat)
        saveChat chat

getAction (LeftChat chat user) config = ExceptT . withLogger $ \logger -> runExceptT $ do
    bot <- getBot config
    when (bot == user) $ liftIO $ do
        logUpdate Logging.debug logger ["LeftChat"] $ "bot left chat: " ++ show (chatId chat)
        deleteChat chat

getAction (NewGroup chat created) config = ExceptT . withLogger $ \logger -> runExceptT $ do
    when created $ liftIO $ do
        logUpdate Logging.debug logger ["NewGroup"] $ "new group chat created " ++ show (chatId chat)
        saveChat chat


fromChat :: Chat -> Group
fromChat = TelegramChat . chatId

saveChat :: Chat -> IO ()
saveChat = saveGroup . fromChat

deleteChat :: Chat -> IO ()
deleteChat = deleteGroup . fromChat