{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}

module UserMapUtils where

import           GHC.Generics         (Generic)

import           Data.Hashable        (Hashable (..))
import           Data.HashMap.Lazy    (HashMap)
import qualified Data.HashMap.Lazy    as H

import qualified Data.ByteString.Lazy as BL
import           Data.Text            (Text)
import qualified Data.Text            as T

import qualified Data.Vector          as V

import qualified Data.Csv             as CSV

data UserInfo = UserInfo
  { sourceUserEmail :: !Text
  , destUserName    :: !Text
  , destAccessToken :: !String
  , destUserEmail   :: !Text
  }
  deriving (Show, Eq, Generic)

type UserName = Text

type UserMap = HashMap UserName UserInfo

type CSVStructure = (Text, Text, Text, Text, String)

readUserMapFile :: FilePath -> IO UserMap
readUserMapFile mapFile = do
  userInfoData <- BL.readFile mapFile
  case CSV.decode CSV.NoHeader userInfoData of
    Left err -> putStrLn err
    Right v  -> pure $ userVectorToMap v

userVectorToMap :: V.Vector CSVStructure -> UserMap
userVectorToMap = vecToHashTable H.empty
  where
    vecToHashTable ht v
      | V.null v  = ht
      | otherwise = let
        (sourceName, destName, sourceEmail, destEmail, token) = V.head v
        userInfo = UserInfo sourceEmail destName token destEmail
          in
            vecToHashTable (H.insert sourceName userInfo ht) (V.tail v)
