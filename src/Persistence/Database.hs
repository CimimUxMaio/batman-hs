module Persistence.Database where

import Data.Set (Set, fromList, insert, delete)
import Model.Group ( Group (chatId) )
import Database.SQLite.Simple ( execute, withConnection, Only (Only), query, query_ )
import Data.Aeson (ToJSON(toJSON), (.:), Value (Object))
import Control.Monad.IO.Class (liftIO)
import Control.Monad (when)
import Logging (prependSections, LoggingFunctionBuilder, withLogger, debug, error, warn)


dbPath :: String
dbPath = "./DB/groups.db"

logDB :: LoggingFunctionBuilder -> LoggingFunctionBuilder
logDB = prependSections ["DB"]

saveGroup :: Group -> IO ()
saveGroup group = withConnection dbPath $ \conn -> withLogger $ \logger -> do
    groups <- getGroups
    if group `notElem` groups
    then do
        logDB Logging.debug logger ["saveGroup"] ("saving " ++ show group ++ " in DB")
        execute conn "INSERT INTO groups (group_type, group_id) VALUES (?, ?);" group
    else logDB Logging.warn logger ["saveGroup"] ("group " ++ show group ++ " already on DB")


deleteGroup :: Group -> IO ()
deleteGroup group = withConnection dbPath $ \conn -> withLogger $ \logger -> do
    logDB Logging.debug logger ["deleteGroup"] ("deleting " ++ show group ++ " from DB")
    execute conn "DELETE FROM groups WHERE group_id = (?);" $ Only (chatId group)


getGroups :: IO [Group]
getGroups = withConnection dbPath $ \conn -> withLogger $ \logger -> do
    logDB Logging.debug logger ["getGroups"] "fetching groups"
    query_ conn "SELECT group_id FROM groups;" :: IO [Group]