{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}


module Main where

import GitHub
import GitHub.Data
import GitHub.Data.Name

import Data.Aeson
import Data.ByteString (ByteString)
import Data.String     (IsString (..))
import Data.Text       (Text)

import Control.Monad.Except
import Control.Monad.Reader

import Configuration.Utils
import Options.Applicative
import PkgInfo_github_migration

import Lens.Micro.TH


data Config = Config
  { _fromURL    :: Text
  , _toURL      :: Text
  , _fromAPIKey :: String
  } deriving Show
$(makeLenses ''Config)

defaultConfig :: Config
defaultConfig = Config
  {_fromURL = "https://github.csiro.au/api/v3"
  ,_toURL= "https://api.github.com/api/v3"
  ,_fromAPIKey=""}


instance FromJSON (Config -> Config) where
  parseJSON = withObject "Config" $ \o -> id
    <$< fromURL    ..: "from-url"     % o
    <*< toURL      ..: "to-url"       % o
    <*< fromAPIKey ..: "from-api-key" % o

instance ToJSON Config where
  toJSON (Config furl turl fkey) = object
    [ "from-url" .= furl
    , "to-url" .= turl
    , "from-api-key" .= fkey
    ]


sflg :: IsString a => Char -> String -> String -> Parser a
sflg c l h = strOption % long l <> short c <> help h

flg :: IsString s => Lens' a s -> Char -> String -> String -> MParser a
flg len c l h = len .:: sflg c l h

pConfig :: MParser Config
pConfig = id
  <$< flg fromURL    'f' "from-url"     "From URL"
  <*< flg toURL      't' "to-url"       "To URL"
  <*< flg fromAPIKey 'k' "from-api-key" "From API Key"

type App a = ReaderT Config (ExceptT Error IO) a

runApp :: Config -> App a -> IO (Either Error a)
runApp conf app = runExceptT $ runReaderT app conf

liftG :: IO (Either Error a) -> App a
liftG = lift . ExceptT

from :: Request x a -> App a
from r = do
  Config{..} <- ask
  liftG (executeRequest (EnterpriseOAuth _fromURL (fromString _fromAPIKey)) r)

mainInfo :: ProgramInfo Config
mainInfo = programInfo "Hello World" pConfig defaultConfig

main :: IO ()
main = runWithPkgInfoConfiguration mainInfo pkgInfo $ \conf -> do
  print conf
  either print print =<< runApp conf (do
    vec <- from $ userEventsR (N "mas17k") FetchAll
    liftIO $ print vec
    )
  print ()
