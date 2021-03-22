{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.Aeson
import Data.Swagger
import Servant.API.Generic
import Data.Text (Text, unpack)
import GHC.Int (Int64)

data Task = Task
  { taskId :: Int64
  , task_name :: Text
  , description :: Text
  , opened :: Bool
  } deriving (Eq, Show, Generic)

instance ToJSON Task where
  toJSON (Task _id _name _description _opened) =
    object [ "id" .= _id
           , "task_name" .= _name
           , "description" .= _description
           , "opened" .= _opened
           ]

instance ToSchema Task

data TaskData = TaskData {
  _name :: Text
, _description :: Text
} deriving (Show, Generic)

instance ToJSON TaskData where
  toJSON (TaskData _n _d) =
    object ["username" .= _n, "description" .= _d]

instance FromJSON TaskData
instance ToSchema TaskData

data User = User
  { userId :: Int64
  , name :: Text
  , isModerator :: Maybe Bool
  , isAdmin :: Maybe Bool
  } deriving (Eq, Show, Generic)


instance ToJSON User where
  toJSON (User _id _name _ _) =
    object [ "id" .= _id
           , "name" .= _name
           ]

instance ToSchema User

data Status = Confirm Text | Err Text
  deriving (Eq, Show, Generic)

instance ToJSON Status where
  toJSON (Confirm t) =
    object [t .= unpack "success"]
  toJSON (Err t) =
    object ["error" .= t]

instance ToSchema Status
