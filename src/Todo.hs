{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Todo where

import Data.Aeson
import Data.Text
import GHC.Generics

-- data User = User
--   { userId :: Text,
--     userName :: Text
--   }

data Todo = Todo
    {
        todoTask :: Text,
        todoDescription :: Text,
        todoDone :: Bool
    }
    deriving (Generic)
-- Data type which describes the request which
-- will be received to create a user
data CreateUpdateTodoRequest = CreateUpdateTodoRequest
  { 
    reqTodoName :: Text,
    reqTodoDescription :: Text
  }
  deriving (Generic)

-- We define a FromJSON instance for CreateUserRequest
-- because we will want to parse it from a HTTP request
-- body (JSON).
instance FromJSON CreateUpdateTodoRequest
instance ToJSON Todo