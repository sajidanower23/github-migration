{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}


module Main where

import GitHub
import GitHub.Data
import GitHub.Data.Name

import           Data.Aeson
import           Data.ByteString (ByteString)
import           Data.Foldable
import           Data.Proxy      (Proxy (..))
import           Data.String     (IsString (..))
import           Data.Text       (Text, isInfixOf, split, unpack)
import           Data.Vector     (Vector)
import qualified Data.Vector     as V

import Control.Monad.Except
import Control.Monad.Reader

import Configuration.Utils
import Options.Applicative
import PkgInfo_github_migration

import Lens.Micro    hiding (Lens', from, to)
import Lens.Micro.TH


data Opts = Opts
  { _fromHost    :: Text
  , _fromAPIKey  :: String
  , _fromRepoStr :: Text
  , _toHost      :: Text
  , _toAPIKey    :: String
  , _toRepoStr   :: Text
  } deriving Show
$(makeLenses ''Opts)

defaultConfig :: Opts
defaultConfig = Opts
  {_fromHost = "https://api.github.com"
  ,_fromAPIKey=""
  ,_fromRepoStr=""
  ,_toHost= "https://api.github.com"
  ,_toAPIKey=""
  ,_toRepoStr=""
  }


instance FromJSON (Opts -> Opts) where
  parseJSON = withObject "Opts" $ \o -> id
    <$< fromHost   ..: "from-host"     % o
    <*< toHost     ..: "to-host"       % o
    <*< fromAPIKey ..: "from-api-key" % o
    <*< toAPIKey   ..: "to-api-key"   % o

instance ToJSON Opts where
  toJSON (Opts fhost fkey frepo thost tkey trepo) = object
    [ "from-host"    .= fhost
    , "from-api-key" .= fkey
    , "from-repo"    .= frepo
    , "to-host"      .= thost
    , "to-api-key"   .= tkey
    , "to-repo"      .= trepo
    ]


sflg :: IsString a => Char -> String -> String -> Parser a
sflg c l h = strOption % long l <> short c <> help h

flg :: IsString s => Lens' a s -> Char -> String -> String -> MParser a
flg len c l h = len .:: sflg c l h

pConfig :: MParser Opts
pConfig = id
  <$< flg fromHost    'f' "from-host"    "From Host"
  <*< flg fromAPIKey  'k' "from-api-key" "From API Key"
  <*< flg fromRepoStr 'r' "from-repo"    "Source Repo"
  <*< flg toHost      't' "to-host"      "To Host"
  <*< flg toAPIKey    'l' "to-api-key"   "To API Key"
  <*< flg toRepoStr   's' "to-repo"      "Dest Repo"


data Config = Config
  {_sourceAuth :: Auth
  ,_destAuth   :: Auth
  ,_sourceRepo :: (Name Owner, Name Repo)
  ,_destRepo   :: (Name Owner, Name Repo)
  }

optsToConfig :: Opts -> Either Text Config
optsToConfig Opts{..} = Config
  <$> makeAuth _fromHost _fromAPIKey
  <*> makeAuth _toHost _toAPIKey
  <*> makeRepo _fromRepoStr
  <*> makeRepo _toRepoStr

makeAuth :: Text -> String -> Either Text Auth
makeAuth host key
  | "api.github.com" `isInfixOf` host = pure (OAuth (fromString key))
  | otherwise = pure (EnterpriseOAuth host (fromString key))

makeRepo :: Text -> Either Text (Name Owner, Name Repo)
makeRepo repostr = case split (=='/') repostr of
  [owner,repo] -> pure (mkName Proxy owner, mkName Proxy repo)
  _ -> Left $ "Repo string not of the form \'<owner>/<repo>\': " <> repostr

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
