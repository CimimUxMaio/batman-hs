module Persistence.Database where

import Data.Set (Set, fromList, insert, delete)
import Model.Group ( Group )


newtype Database = Database { groups :: Set Group } deriving Show


init :: Database
init = Database { groups = fromList [] }


addGroup :: Group -> Database -> Database
addGroup group db = db { groups = insert group . groups $ db }

removeGroup :: Group -> Database -> Database
removeGroup group db = db { groups = delete group . groups $ db }