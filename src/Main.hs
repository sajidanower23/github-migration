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
import Data.Foldable
import Data.String     (IsString (..))
import Data.Text       (Text)

import Control.Monad.Except
import Control.Monad.Reader

import Configuration.Utils
import Options.Applicative
import PkgInfo_github_migration

import Lens.Micro    hiding (Lens', from, to)
import Lens.Micro.TH


data Config = Config
  { _fromURL    :: Text
  , _fromAPIKey :: String
  , _toURL      :: Text
  , _toAPIKey   :: String
  } deriving Show
$(makeLenses ''Config)

defaultConfig :: Config
defaultConfig = Config
  {_fromURL = "https://github.csiro.au/api/v3"
  ,_fromAPIKey=""
  ,_toURL= "https://api.github.com/api/v3"
  ,_toAPIKey=""
  }


instance FromJSON (Config -> Config) where
  parseJSON = withObject "Config" $ \o -> id
    <$< fromURL    ..: "from-url"     % o
    <*< toURL      ..: "to-url"       % o
    <*< fromAPIKey ..: "from-api-key" % o
    <*< toAPIKey   ..: "to-api-key"   % o

instance ToJSON Config where
  toJSON (Config furl fkey turl tkey) = object
    [ "from-url" .= furl
    , "to-url" .= turl
    , "from-api-key" .= fkey
    , "to-api-key" .= tkey
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
  <*< flg toAPIKey   'l' "to-api-key"   "To API Key"

type App a = ReaderT Config (ExceptT Error IO) a

runApp :: Config -> App a -> IO (Either Error a)
runApp conf app = runExceptT $ runReaderT app conf

-- Run a github action in the App monad
liftG :: IO (Either Error a) -> App a
liftG = lift . ExceptT

-- Run a request on the 'from' server
from :: Request x a -> App a
from r = do
  Config{..} <- ask
  liftIO (print r)
  liftG (executeRequest (EnterpriseOAuth _fromURL (fromString _fromAPIKey)) r)

-- Run a request on the 'to' server
to :: Request x a -> App a
to r = do
  Config{..} <- ask
  liftIO (print r)
  liftG (executeRequest (OAuth (fromString _toAPIKey)) r)

mainInfo :: ProgramInfo Config
mainInfo = programInfo "Hello World" pConfig defaultConfig

main :: IO ()
main = runWithPkgInfoConfiguration mainInfo pkgInfo $ \conf -> do
  print conf
  either print print =<< runApp conf (do
    -- vec <- from $ userEventsR (N "Axman6") FetchAll
    -- liftIO $ print vec
    vec <- from $ issuesForRepoR  (N "axman6") (N "test-from") stateAll FetchAll
    liftIO (print vec)
    traverse_ moveIssue vec
    rateLimitCore <$> from rateLimitR
    )
  pure ()


moveIssue :: Issue -> App Issue
moveIssue iss = to $ createIssueR (N "axman6") (N "test-to") $ NewIssue
  { newIssueTitle = (issueTitle iss)
  , newIssueBody = (<> ("\n\n_(Moved with "<> pkgInfo ^. _3 <> ")_")) <$> issueBody iss
  , newIssueLabels = Just (labelName <$> issueLabels iss)
  , newIssueAssignee = Nothing
  , newIssueMilestone = Nothing
  }
