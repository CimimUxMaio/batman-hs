module Persistence.Database where

import Data.Set (Set)
import Model.Group ( Group )

newtype Database = Database { groups :: Set Group }