{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}


module Main where

import GitHub
import GitHub.Data.Id
import GitHub.Data.Name (Name (..))
import GitHub.Data.Options

import Data.Aeson
import Data.Foldable
import Data.Proxy    (Proxy (..))
import Data.String   (IsString (..))
import Data.Text     (Text, isInfixOf, split, unpack)

import qualified Data.Vector as V

import Control.Monad.Except
import Control.Monad.Reader

import Configuration.Utils
import Options.Applicative
import PkgInfo_github_migration

import Lens.Micro    hiding (Lens')
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
source :: Request x a -> App a
source r = do
  Config{..} <- ask
  liftIO (print r)
  liftG (executeRequest _sourceAuth r)

sourceRepo :: (Name Owner -> Name Repo -> a) -> (a -> Request x b) -> App b
sourceRepo f g = do
  f' <- uncurry f <$> asks _sourceRepo
  source (g f')

-- Run a request on the 'to' server
dest :: Request x a -> App a
dest r = do
  Config{..} <- ask
  liftIO (print r)
  liftG (executeRequest _destAuth r)

destRepo :: (Name Owner -> Name Repo -> a) -> (a -> Request x b) -> App b
destRepo f g = do
  f' <- uncurry f <$> asks _destRepo
  dest (g f')

mainInfo :: ProgramInfo Opts
mainInfo = programInfo "github-migration" pConfig defaultConfig

main :: IO ()
main = runWithPkgInfoConfiguration mainInfo pkgInfo $ \opts -> do
  print opts
  res <- optsToConfig opts & either (error . show) (\conf -> runApp conf $ do

    -- transferLabels
    transferIssues
    getPulls
    -- transferMilestones
    -- vec <- source =<< (sourceRepo issuesForRepoR $ \f -> f (stateAll<>sortAscending) FetchAll)
    -- liftIO (mapM_ (print . issueNumber) vec)
    -- traverse_ transferIssue vec
    rateLimitCore <$> source rateLimitR
    )
  either print print res
  pure ()

getPulls :: App ()
getPulls = do
  pulls <- sourceRepo pullRequestsForR $ \f -> f stateAll FetchAll
  traverse_ (liftIO . print) pulls

transferIssues :: App ()
transferIssues = do
  issues <- sourceRepo issuesForRepoR $ \f -> f (stateAll<>sortAscending) FetchAll
  traverse_ (liftIO . print . issuePullRequest) issues
  where
    transferIssue :: Issue -> App ()
    transferIssue iss = do
      let (N authorName) = simpleUserLogin . issueUser $ iss
      destRepo createIssueR ($ NewIssue
          { newIssueTitle     = issueTitle iss
          , newIssueBody      = (<> ("\n\n_Original Author: " <> authorName <> "_\n\n_(Moved with "<> pkgInfo ^. _3 <> ")_")) <$> issueBody iss
          , newIssueLabels    = Just (labelName <$> issueLabels iss)
          , newIssueAssignees = if V.null (issueAssignees iss)
                                  then mempty
                                  else simpleUserLogin <$> issueAssignees iss
          , newIssueMilestone = Nothing -- TODO: milestoneNumber <$> issueMilestone iss
          })
      transferIssueComments iss

transferIssueComments :: Issue -> App ()
transferIssueComments iss = do
  let iid = Id $ issueNumber iss
  -- liftIO $ print $ "Listing the issue comments of iss id : " ++ iid
  cmnts <- sourceRepo commentsR $ \f -> f iid FetchAll
  traverse_ (transferSingleComment iid) cmnts
  where
    transferSingleComment :: Id Issue -> IssueComment -> App Comment
    transferSingleComment iid cmnt =
      let (N authorName) = simpleUserLogin . issueCommentUser $ cmnt
          oldCmntBody = issueCommentBody cmnt
          newCommentBody = oldCmntBody <> "\n\n_Original Author: " <> authorName <> "_\n"
        in
          destRepo createCommentR $ \f -> f iid newCommentBody

transferLabels :: App ()
transferLabels = do
  sourcelbls <- sourceRepo labelsOnRepoR ($ FetchAll)
  -- liftIO $ mapM_ print sourcelbls
  destlbls <- destRepo labelsOnRepoR ($ FetchAll)
  let (exist, create) = V.partition (\l -> labelName l `V.elem` (labelName <$> destlbls)) sourcelbls
  traverse_ moveLabel create
  traverse_ updateLabel exist
  where
    moveLabel :: IssueLabel -> App IssueLabel
    moveLabel lbl = destRepo createLabelR $ \f ->
        f  (labelName lbl) (unpack $ labelColor lbl)

    updateLabel :: IssueLabel -> App IssueLabel
    updateLabel lbl = destRepo updateLabelR  $ \f ->
        f (labelName lbl) (labelName lbl) (unpack $ labelColor lbl)

transferMilestones :: App ()
transferMilestones = do
  sourceMilestones <- sourceRepo milestonesR ($ FetchAll)
  liftIO $ mapM_ print sourceMilestones
  traverse_ transferMilestone sourceMilestones
  where
    milestoneToNewMilestone :: Milestone -> NewMilestone
    milestoneToNewMilestone mlstn =
      NewMilestone
        { newMilestoneTitle = milestoneTitle mlstn
        , newMilestoneState = milestoneState mlstn
        , newMilestoneDescription = milestoneDescription mlstn
        , newMilestoneDueOn = milestoneDueOn mlstn
        }

    transferMilestone :: Milestone -> App Milestone
    transferMilestone mlstn = destRepo createMilestoneR ($ milestoneToNewMilestone mlstn)
