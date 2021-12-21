module Persistence.Database where

import Data.Set (Set, fromList, insert)
import Model.Group ( Group )


newtype Database = Database { groups :: Set Group }


init :: Database
init = Database { groups = fromList [] }


addGroup :: Group -> Database -> Database
addGroup group db = db { groups = insert group . groups $ db }