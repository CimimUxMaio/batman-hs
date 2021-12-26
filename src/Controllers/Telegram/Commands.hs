module Controllers.Telegram.Commands where

import Telegram.Types ( Message )
import Data.List (intercalate)
import Logging (withLogger)
import qualified Logging
import Controllers.Telegram.LogHelper (logCommand)


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


command name args = withLogger $ \logger -> do  -- Default command
    logCommand Logging.debug logger ["unknownCommand"] cmdDesc
    pure . ResponseMessage $ cmdDesc
    where wrap c s = c:s ++ [c]
          cmdDesc = "unknown command "
                  ++ wrap '"' name
                  ++ " with args: "
                  ++ intercalate ", " args