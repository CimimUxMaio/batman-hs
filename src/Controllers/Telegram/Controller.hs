module Controllers.Telegram.Controller
    ( controller ) where

import Controllers.Telegram.Update
    ( Update(message),
      Message(NewGroup, TextMessage, NewChatMembers, LeftChat), Chat (chatId), User )
import Config ( TelegramConfig(token) )
import Data.IORef ( IORef )
import Persistence.Database ( Database )
import Web.Scotty
    ( capture, jsonData, liftAndCatchIO, post, ActionM, ScottyM )
import Controllers.Telegram.Commands
    ( Command,
      CommandResponse(ResponseMessage, NoResponse),
      CommandArgs, parseCommand )
import Telegram ( sendMessage )
import Data.Functor ((<&>))


controller :: TelegramConfig -> IORef Database -> ScottyM ()
controller config dbRef = do
    let token = Config.token config
    let webHook = '/' : token
    post (capture webHook) $ do
        body <- (jsonData :: ActionM Update)
        case message body of
            Nothing  -> pure ()  -- Never happens (?)
            Just msg -> liftAndCatchIO $ getAction msg config dbRef



{- HANDLERS -}

type MessageAction = TelegramConfig -> IORef Database -> IO ()

getAction :: Message -> MessageAction
getAction (TextMessage chat msgText) config dbRef = do
    let action = parseCommand msgText <&> uncurry ($)
    case action of
        Nothing    -> pure ()
        Just doAction -> do
            response <- doAction
            handleResponse (chatId chat) response
    where handleResponse _ NoResponse = pure ()
          handleResponse chatId (ResponseMessage msg) = sendMessage config chatId msg

getAction (NewChatMembers chat users) config dbRef = 
    print $ "added to chat " ++ show users
    
getAction (LeftChat chat user) config dbRef = 
    print $ "removed from chat " ++ show user 

getAction (NewGroup chat created) config dbRef = 
    print $ "new group chat " ++ show created