module Controllers.Telegram.Update where

import GHC.Generics (Generic)
import Data.Aeson ( FromJSON(parseJSON), (.:), (.:?), withObject )
import Data.Foldable (asum)



newtype User = User { userId :: Int } deriving (Show, Generic)

instance FromJSON User where
    parseJSON = withObject "User" $ \obj ->
        User <$> obj .: "id"


newtype Chat = Chat { chatId :: Int } deriving (Show, Generic)

instance FromJSON Chat where
    parseJSON = withObject "Chat" $ \obj ->
        Chat <$> obj .: "id"


data Message = TextMessage { chat :: Chat, msgText :: String } 
             | NewChatMembers { chat :: Chat, users :: [User] }
             | LeftChat { chat :: Chat, user :: User }
             | NewGroup { chat :: Chat, created :: Bool }
             deriving (Show, Generic)
            
instance FromJSON Message where
    parseJSON = withObject "Message" $ \obj -> do 
        let chat = obj .: "chat"
        asum [
            TextMessage    <$> chat <*> obj .: "text",
            NewChatMembers <$> chat <*> obj .: "new_chat_members",
            LeftChat       <$> chat <*> obj .: "left_chat_member",
            NewGroup       <$> chat <*> obj .: "group_chat_created" ]


newtype Update = Update { message :: Maybe Message } deriving (Show, Generic, FromJSON)