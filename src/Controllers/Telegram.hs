module Controllers.Telegram
    ( controller ) where

import Web.Scotty
    ( capture, jsonData, liftAndCatchIO, post, ActionM, ScottyM, body )

import GHC.Generics (Generic)
import Data.Aeson (FromJSON)
import Data.Functor ( (<&>) )
import Data.List (intercalate)
import TelegramChat
import Prelude hiding (id)
import Config (TelegramConfig)
import qualified Config
import Control.Monad.Trans.Maybe ( MaybeT )


newtype Chat = Chat { id :: Integer } deriving (Show, Generic)
data Message = Message { chat :: Chat
                       , text :: String } deriving (Show, Generic)
newtype Update = Update { message :: Maybe Message } deriving (Show, Generic)

instance FromJSON Chat
instance FromJSON Message
instance FromJSON Update


type CommandContext = Message
type CommandArgs = [String]
data CommandResponse = ResponseMessage String | NoResponse
type Command = CommandArgs -> IO CommandResponse


controller :: TelegramConfig -> ScottyM ()
controller config = do
    let token = Config.token config
    let webHook = '/' : token
    post (capture webHook) $ do
        body <- (jsonData :: ActionM Update)
        case message body of
            Nothing  -> pure () -- Should register new chat?
            Just msg -> liftAndCatchIO $ handleMessage config msg


{- HANDLERS -}

handleMessage :: TelegramConfig -> Message -> IO ()
handleMessage config msg = do
    let msgText = text msg
    let action = parseCommand msgText <&> uncurry ($)
    case action of
        Nothing    -> pure ()
        Just doAction -> do
            let chatId = id . chat $ msg
            response <- doAction
            handleResponse chatId response
    
    where handleResponse _ NoResponse = pure ()
          handleResponse chatId (ResponseMessage msg) = sendMessage config chatId msg



parseCommand :: String -> Maybe (Command, CommandArgs)
parseCommand ('/':text) = Just (command name, args)
    where name = head . words $ text
          args = tail . words $ text
parseCommand _ = Nothing  -- Just a message


command :: String -> Command
command "hello" args =
    pure . ResponseMessage $ "Hello " ++ unwords args ++ "!"

command name args =  -- Default command
    pure . ResponseMessage $ "unknown command "
                          ++ wrap '"' name
                          ++ " with args: "
                          ++ intercalate ", " args
    where wrap c s = c:s ++ [c]