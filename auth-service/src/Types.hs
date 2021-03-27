{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.Aeson
import Data.Swagger
import Servant.API.Generic
import Data.Text (Text, unpack)
import GHC.Int (Int64)

type UserId = Int64

data User = User
  { userId :: UserId
  , name :: Text
  , isModerator :: Maybe Bool
  , isAdmin :: Maybe Bool
  , pass :: Text
  } deriving (Eq, Show, Generic)

instance ToJSON User where
  toJSON (User _id _name _ _ _) =
    object [ "id" .= _id
           , "name" .= _name
           ]

instance ToSchema User

data AuthData = AuthData {
  login :: Text
, password :: Text
} deriving (Show, Generic)

instance ToJSON AuthData where
  toJSON (AuthData _log _pass) =
    object ["username" .= _log, "pass" .= _pass]

instance FromJSON AuthData
instance ToSchema AuthData

data Status = Confirm Text | Err Text
  deriving (Eq, Show, Generic)

instance ToJSON Status where
  toJSON (Confirm t) =
    object [t .= unpack "success"]
  toJSON (Err t) =
    object ["error" .= t]

instance ToSchema Status
