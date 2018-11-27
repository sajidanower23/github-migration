{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}

module UserMapUtils where

import GHC.Generics (Generic)

import Data.HashMap.Lazy as H

import Data.Aeson

import Data.Text as T

data UserInfo = UserInfo
  { sourceAccessToken :: String
  , sourceUserEmail   :: Text
  , destUserName      :: !Text
  , destAccessToken   :: String
  , destUserEmail     :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON UserInfo where
  parseJSON (Object v) =
    UserInfo
      <$> v .: "access_token_source"
      <*> v .: "user_email_source"
      <*> v .: "user_name_dest"
      <*> v .: "access_token_dest"
      <*> v .: "user_email_dest"
  parseJSON _ = mempty

instance ToJSON UserInfo where
  toJSON (UserInfo sourceToken sourceEmail destName destToken destEmail) = object
    [ "access_token_source" .= sourceToken
    , "user_email_source"   .= sourceEmail
    , "user_name_dest"      .= destName
    , "access_token_dest"   .= destToken
    , "user_email_dest"     .= destEmail
    ]

newtype UserName = UserName Text
  deriving (Eq)
deriving instance Show UserName

type UserMap = HashMap UserName UserInfo
