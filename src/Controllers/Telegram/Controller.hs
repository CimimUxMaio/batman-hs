module Controllers.Telegram.Controller
    ( controller ) where

import Telegram.Types
    ( Update(message),
      Message(NewGroup, TextMessage, NewChatMembers, LeftChat, chat), Chat (chatId), User )
import Config ( TelegramConfig(token) )
import Data.IORef ( IORef, readIORef, modifyIORef )
import Persistence.Database ( Database, addGroup, removeGroup )
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



controller :: TelegramConfig -> IORef Database -> ScottyM ()
controller config dbRef = do
    let token = Config.token config
    let webHook = '/' : token
    post (capture webHook) $ do
        body <- (jsonData :: ActionM Update)
        liftIO $ withLogger $ \logger -> do
            let msg = message body
            res <- runExceptT $ getAction msg config dbRef
            case res of
                Left e  -> logUpdate Logging.error logger [] $ show e
                Right _ -> pure ()


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

getAction (NewChatMembers chat users) config dbRef = ExceptT . withLogger $ \logger -> runExceptT $ do
    bot <- getBot config
    when (bot `elem` users) $ liftIO $ do
        saveChat chat dbRef
        logUpdate Logging.debug logger ["NewChatMembers"] $ "bot added to chat " ++ show (chatId chat)

getAction (LeftChat chat user) config dbRef = ExceptT . withLogger $ \logger -> runExceptT $ do
    bot <- getBot config
    when (bot == user) $ liftIO $ do
        deleteChat chat dbRef
        logUpdate Logging.debug logger ["LeftChat"] $ "bot removed from chat " ++ show (chatId chat)

getAction (NewGroup chat created) config dbRef = ExceptT . withLogger $ \logger -> runExceptT $ do
    when created $ liftIO $ do
        saveChat chat dbRef
        logUpdate Logging.debug logger ["NewGroup"] $ "bot added to chat " ++ show (chatId chat)


saveChat :: Chat -> IORef Database -> IO ()
saveChat chat dbRef = modifyIORef dbRef $ addGroup newGroup
    where newGroup = TelegramChat . chatId $ chat


deleteChat :: Chat -> IORef Database -> IO ()
deleteChat chat dbRef = modifyIORef dbRef $ removeGroup targetGroup
    where targetGroup = TelegramChat . chatId $ chat