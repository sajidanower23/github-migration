{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Main where

import           GHC.Generics             (Generic)

import           GitHub
import           GitHub.Data.Id
import           GitHub.Data.Name         (Name (..))

import           Data.Aeson
import           Data.Foldable
import           Data.Proxy               (Proxy (..))
import           Data.String              (IsString (..))
import           Data.Text                (Text, isInfixOf, split, unpack)
import qualified Data.Text                as T

import           Control.Monad.Except
import           Control.Monad.Reader

import           Configuration.Utils
import           Options.Applicative
import           PkgInfo_github_migration

import           Lens.Micro               hiding (Lens')
import           Lens.Micro.TH

import           Data.Hashable            (Hashable (..))
import           Data.HashMap.Lazy        (HashMap)
import qualified Data.HashMap.Lazy        as H

import qualified Data.ByteString.Lazy     as BL

import qualified Data.Vector              as V

import qualified Data.Csv                 as CSV


-- ============ Command Line Args/Config =================

data Opts = Opts
  { _fromHost    :: Text
  , _fromAPIKey  :: String
  , _fromRepoStr :: Text
  , _toHost      :: Text
  , _toAPIKey    :: String
  , _toRepoStr   :: Text
  , _userMapFile :: FilePath
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
  ,_userMapFile=""
  }


instance FromJSON (Opts -> Opts) where
  parseJSON = withObject "Opts" $ \o -> id
    <$< fromHost    ..: "from-host"     % o
    <*< toHost      ..: "to-host"       % o
    <*< fromAPIKey  ..: "from-api-key"  % o
    <*< toAPIKey    ..: "to-api-key"    % o
    <*< userMapFile ..: "user-map-file" % o

instance ToJSON Opts where
  toJSON (Opts fhost fkey frepo thost tkey trepo mapFile) = object
    [ "from-host"     .= fhost
    , "from-api-key"  .= fkey
    , "from-repo"     .= frepo
    , "to-host"       .= thost
    , "to-api-key"    .= tkey
    , "to-repo"       .= trepo
    , "user-map-file" .= mapFile
    ]


sflg :: IsString a => Char -> String -> String -> Parser a
sflg c l h = strOption % long l <> short c <> help h

flg :: IsString s => Lens' a s -> Char -> String -> String -> MParser a
flg len c l h = len .:: sflg c l h

pConfig :: MParser Opts
pConfig = id
  <$< flg fromHost    'f' "from-host"     "From Host"
  <*< flg fromAPIKey  'k' "from-api-key"  "From API Key"
  <*< flg fromRepoStr 'r' "from-repo"     "Source Repo"
  <*< flg toHost      't' "to-host"       "To Host"
  <*< flg toAPIKey    'l' "to-api-key"    "To API Key"
  <*< flg toRepoStr   's' "to-repo"       "Dest Repo"
  <*< flg userMapFile 'c' "user-map-file" "CSV File containing user maps"

-- ============ User Types =================

type UserName = Text

type UserAuthMap = HashMap UserName Auth

-- | A map between source and destination usernames
type UserNameMap = HashMap UserName UserName
--                         Source   Dest

-- ============ CSV Reader Utils =================

type CSVStructure = (Text, Text, Text, Text, String)

readUserMapFile :: FilePath -> IO (V.Vector CSVStructure)
readUserMapFile mapFile = do
  userInfoData <- BL.readFile mapFile
  case CSV.decode CSV.NoHeader userInfoData of
    Left err -> error $ "Could not read CSV: " <> err
    Right v  -> pure v

userVToAuthMap :: Text -> V.Vector CSVStructure -> UserAuthMap
userVToAuthMap host = vecToAuthTable host H.empty
  where
    vecToAuthTable host ht v
      | V.null v  = ht
      | otherwise =
          let (sourceName, _, _, _, token) = V.head v
              eAuth = makeAuth host token
          in
          case eAuth of
            Left err -> error $ "Error while parsing user data: " <> show err
            Right auth -> vecToAuthTable host (H.insert sourceName auth ht) (V.tail v)

userVToNameMap :: V.Vector CSVStructure -> UserNameMap
userVToNameMap = makeNameTable H.empty
  where
    makeNameTable ht v
      | V.null v  = ht
      | otherwise =
        let (sourceName, destName, _, _, _) = V.head v in
          makeNameTable (H.insert sourceName destName ht) (V.tail v)

-- ============ App Config/State =================

data Config = Config
  {_sourceAuth  :: Auth
  ,_destAuth    :: Auth
  ,_userAuthMap :: UserAuthMap
  ,_userInfoMap :: UserNameMap
  ,_sourceRepo  :: (Name Owner, Name Repo)
  ,_destRepo    :: (Name Owner, Name Repo)
  }

optsToConfig :: UserNameMap -> UserAuthMap -> Opts -> Either Text Config
optsToConfig userNameHt authHt Opts{..} = Config
  <$> makeAuth _fromHost _fromAPIKey
  <*> makeAuth _toHost _toAPIKey
  <*> Right authHt
  <*> Right userNameHt
  <*> makeRepoName _fromRepoStr
  <*> makeRepoName _toRepoStr

makeAuth :: Text -> String -> Either Text Auth
makeAuth host key
  | "api.github.com" `isInfixOf` host = pure (OAuth (fromString key))
  | otherwise = pure (EnterpriseOAuth host (fromString key))

makeRepoName :: Text -> Either Text (Name Owner, Name Repo)
makeRepoName repostr = case split (=='/') repostr of
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

destWithAuth :: UserName -> Request x a -> App a
destWithAuth username r = do
  Config fromAuth toAuth authMap nameMap fromRepo toRepo <- ask
  liftIO (print r)
  let mUserInfo = H.lookup username authMap
  case mUserInfo of
    Nothing -> dest r -- defaulting to dest without auth
    Just auth -> do
      liftG (executeRequest auth r)

destRepoWithAuth :: UserName -> (Name Owner -> Name Repo -> a) -> (a -> Request x b) -> App b
destRepoWithAuth username f g = do
  f' <- uncurry f <$> asks _destRepo
  destWithAuth username (g f')


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
  userV <- readUserMapFile (_userMapFile opts)
  let authHt = userVToAuthMap (_toHost opts) userV
      userNameHt = userVToNameMap userV
  res <- (optsToConfig userNameHt authHt) opts & either (error . show) (\conf -> runApp conf $ do
    transferLabels
    transferMilestones
    transferIssues
    -- vec <- source =<< (sourceRepo issuesForRepoR $ \f -> f (stateAll<>sortAscending) FetchAll)
    -- liftIO (mapM_ (print . issueNumber) vec)
    -- traverse_ transferIssue vec
    rateLimitCore <$> source rateLimitR
    )
  either print print res
  pure ()

-- ============ Transfer Utils =================

-- | Lookup what each user in source is called in dest
getDestAssignees :: V.Vector UserName -> App (V.Vector UserName)
getDestAssignees sourceAssignees = do
  userNameHt <- asks _userInfoMap
  findDestAssignee userNameHt V.empty sourceAssignees
  where
    findDestAssignee nameHt acc v
      | V.null v = pure acc
      | otherwise =
          case H.lookup (V.head v) nameHt of
            Nothing -> findDestAssignee nameHt acc (V.tail v)
            Just dName -> findDestAssignee nameHt (V.snoc acc dName) (V.tail v)

-- | Accessor for the newtype ``Name`` defined in GitHub.Data.Name
getName :: Name entity -> Text
getName (N t) = t

transferIssues :: App ()
transferIssues = do
  vec <- sourceRepo issuesForRepoR $ \f -> f (stateAll<>sortAscending) FetchAll
  traverse_ transferIssue vec
  where
    transferIssue :: Issue -> App ()
    transferIssue iss = do
      let authorName = getName . simpleUserLogin . issueUser $ iss
          sourceAssigneeNames = getName . simpleUserLogin <$> issueAssignees iss
      destAssignees <- getDestAssignees sourceAssigneeNames
      destRepoWithAuth authorName createIssueR ($ NewIssue
          { newIssueTitle     = issueTitle iss
          , newIssueBody      = (<> ("\n\n_Original Author: " <> authorName <> "_\n\n_(Moved with "<> pkgInfo ^. _3 <> ")_")) <$> issueBody iss
          , newIssueLabels    = Just (labelName <$> issueLabels iss)
          , newIssueAssignees = if V.null (issueAssignees iss)
                                  then mempty
                                  else N <$> destAssignees
          , newIssueMilestone = milestoneNumber <$> issueMilestone iss
          })
      transferIssueComments iss

transferIssueComments :: Issue -> App ()
transferIssueComments iss = do
  let iid = Id $ issueNumber iss
  cmnts <- sourceRepo commentsR $ \f -> f iid FetchAll
  traverse_ (transferSingleComment iid) cmnts
  where
    transferSingleComment :: Id Issue -> IssueComment -> App Comment
    transferSingleComment iid cmnt =
      let (N authorName) = simpleUserLogin . issueCommentUser $ cmnt
          oldCmntBody = issueCommentBody cmnt
          newCommentBody = oldCmntBody <> "\n\n_Original Author: " <> authorName <> "_\n"
        in
          destRepoWithAuth authorName createCommentR $ \f -> f iid newCommentBody

transferLabels :: App ()
transferLabels = do
  sourcelbls <- sourceRepo labelsOnRepoR ($ FetchAll)
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
    transferMilestone mlstn =
      let (N authorName) = simpleUserLogin . milestoneCreator $ mlstn in
      destRepoWithAuth authorName createMilestoneR ($ milestoneToNewMilestone mlstn)
