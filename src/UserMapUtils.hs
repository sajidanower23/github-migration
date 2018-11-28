{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}

module UserMapUtils where

import           GHC.Generics         (Generic)

import           Data.HashMap.Lazy    as H

import           Data.Aeson

import qualified Data.ByteString.Lazy as BL
import           Data.Hashable        (Hashable (..))
import           Data.Text            as T
import qualified Data.Vector          as V

import qualified Data.Csv             as CSV

data UserInfo = UserInfo
  { sourceAccessToken :: !String
  , sourceUserEmail   :: !Text
  , destUserName      :: !Text
  , destAccessToken   :: !String
  , destUserEmail     :: !Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON UserInfo where
  parseJSON = withObject "UserInfo" $ \u -> UserInfo
      <$> u .: "access_token_source"
      <*> u .: "user_email_source"
      <*> u .: "user_name_dest"
      <*> u .: "access_token_dest"
      <*> u .: "user_email_dest"

instance ToJSON UserInfo where
  toJSON (UserInfo sourceToken sourceEmail destName destToken destEmail) = object
    [ "access_token_source" .= sourceToken
    , "user_email_source"   .= sourceEmail
    , "user_name_dest"      .= destName
    , "access_token_dest"   .= destToken
    , "user_email_dest"     .= destEmail
    ]

newtype UserName = UserName { getUserName :: Text }
  deriving (Eq)
deriving instance Show UserName
deriving instance Hashable UserName

readUserMapFile :: FilePath -> IO ()
readUserMapFile mapFile = do
  userInfoData <- BL.readFile mapFile
  case CSV.decode CSV.NoHeader userInfoData of
    Left err -> putStrLn err
    Right v -> V.forM_ v $ \(sourceName, destName, sourceEmail, destEmail, token) ->
      error "not implemented yet"

type UserMap = HashMap UserName UserInfo
