module Controllers.Telegram.Commands where

import Controllers.Telegram.Update ( Message )
import Data.List (intercalate)


type CommandContext = Message
type CommandArgs = [String]
data CommandResponse = ResponseMessage String | NoResponse
type Command = CommandArgs -> IO CommandResponse


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