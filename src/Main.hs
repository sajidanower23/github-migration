{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Main where

import GHC.Generics (Generic)

import GitHub                               hiding (Status)
import GitHub.Data.Id
import GitHub.Data.Name                     (Name (..))
import GitHub.Endpoints.Issues              (editOfIssue)
import GitHub.Endpoints.Repos.Collaborators (addCollaboratorR)

import Control.Concurrent (threadDelay)

import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time.LocalTime

import           Formatting      hiding ((%))
import qualified Formatting      as F
import           Formatting.Time

import Network.HTTP.Client       (HttpException (..), HttpExceptionContent (..),
                                  responseHeaders, responseStatus)
import Network.HTTP.Types.Status (Status (..))

import           Data.Aeson
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BS8
import           Data.Foldable
import           Data.List             (sortOn)
import           Data.Proxy            (Proxy (..))
import           Data.String           (IsString (..))
import           Data.Text             (Text, isInfixOf, split, unpack)
import qualified Data.Text             as T

import Control.Monad.Except
import Control.Monad.Reader

import Configuration.Utils
import Options.Applicative
import PkgInfo

import Lens.Micro    hiding (Lens')
import Lens.Micro.TH

import           Data.Hashable     (Hashable (..))
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as H

import qualified Data.ByteString.Lazy as BL

import qualified Data.Vector as V

import qualified Data.Csv as CSV

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
  <*< flg userMapFile 'm' "user-map-file" "CSV File containing user maps"

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
  case CSV.decode CSV.HasHeader userInfoData of
    Left err -> error $ "Could not read CSV: " <> err
    Right v  -> pure v

userVToAuthMap :: Text -> V.Vector CSVStructure -> UserAuthMap
userVToAuthMap host =
    H.fromList
  . map (\(sourceName,_,_,_,token) -> (sourceName, handleAuthError (makeAuth host token)))
  . V.toList
  where
    handleAuthError :: Either Text Auth -> Auth
    handleAuthError =
        either
          (\err -> error $ "Error while constructing auth data: " <> show err)
          id

userVToNameMap :: V.Vector CSVStructure -> UserNameMap
userVToNameMap =
  H.fromList . map (\(sourceName, destName,_,_,_) -> (sourceName, destName)) . V.toList

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
source :: FromJSON a => Request x a -> App a
source r = handleAbuseErrors $ do
  Config{..} <- ask
  liftIO (print r)
  liftG (executeRequest _sourceAuth r)

sourceRepo :: FromJSON b => (Name Owner -> Name Repo -> a) -> (a -> Request x b) -> App b
sourceRepo f g = do
  f' <- uncurry f <$> asks _sourceRepo
  source (g f')

destWithAuth :: ParseResponse t a => UserName -> GenRequest t x a -> App a
destWithAuth sourceName r = handleAbuseErrors $ do
  authMap <- asks _userAuthMap
  liftIO (print r)
  let mUserInfo = H.lookup sourceName authMap
  case mUserInfo of
    Nothing -> do
      liftIO (print $ "Could not find auth map for username: " <> sourceName <> ", using default auth")
      dest r -- defaulting to dest without auth
    Just auth -> do
      liftIO (print $ "Request being executed as " <> sourceName)
      liftG (executeRequest auth r) `catchError` \e -> do
        liftIO . print $ "Encountered error: " <> show e
        liftIO . print $ "Using default auth"
        dest r

destRepoWithAuth :: ParseResponse t b => UserName -> (Name Owner -> Name Repo -> a) -> (a -> GenRequest t x b) -> App b
destRepoWithAuth username f g = do
  f' <- uncurry f <$> asks _destRepo
  destWithAuth username (g f')


-- Run a request on the 'to' server
dest :: ParseResponse t a => GenRequest t x a -> App a
dest r = handleAbuseErrors $ do
  Config{..} <- ask
  liftIO (print r)
  liftG (executeRequest _destAuth r)

destRepo ::  ParseResponse t b => (Name Owner -> Name Repo -> a) -> (a -> GenRequest t x b) -> App b
destRepo f g = do
  f' <- uncurry f <$> asks _destRepo
  dest (g f')


handleAbuseErrors :: App a -> App a
handleAbuseErrors m = go 0 where
  go n | n > 3 = throwError (UserError "Request was blocked three times for abuse, failing.")
       | otherwise = m `catchError` \e -> case e of -- My kingdom for some prisms
          HTTPError (HttpExceptionRequest _req (StatusCodeException resp msg)) ->
            case responseStatus resp of
              Status{statusCode = 403, statusMessage = "Forbidden"} ->
                case lookup "X-RateLimit-Reset" (responseHeaders resp) of
                  Just posixSecsBS -> case reads (BS8.unpack posixSecsBS) of
                    [(posixSecs::Int,"")] -> do
                      liftIO $ do
                        nowP <- getPOSIXTime
                        tz <- getCurrentTimeZone
                        -- Time divided by 3 because github lets you start
                        -- posting again sooner than the reset time
                        let waitTime = (fromIntegral posixSecs - nowP) / (max 1 (3 - n)) + 2
                        fprint ("Abuse limit triggered, delaying for " F.% diff False
                               F.% " (Until " F.% hmsPL F.% ")\n")
                              waitTime
                              (utcToZonedTime tz (addUTCTime waitTime (posixSecondsToUTCTime nowP)))
                        threadDelay (ceiling waitTime * 10^6)
                      go (n+1)
                    _ -> throwError e
                  _ -> throwError e
              _ -> throwError e
          _ -> throwError e

mainInfo :: ProgramInfo Opts
mainInfo = programInfo "github-migration" pConfig defaultConfig

main :: IO ()
main = runWithPkgInfoConfiguration mainInfo pkgInfo $ \opts -> do
  print opts
  userV <- readUserMapFile (_userMapFile opts)
  let authHt = userVToAuthMap (_toHost opts) userV
      userNameHt = userVToNameMap userV
  res <- optsToConfig userNameHt authHt opts & either (error . show) (\conf -> runApp conf $ do
    inviteDestUsers
    transferLabels
    transferMilestones
    transferIssues
    )
  either print print res
  pure ()

-- ============ User Utils =================

inviteDestUsers :: App ()
inviteDestUsers = do
  liftIO $ putStrLn "Inviting users"
  nameMap <- asks _userInfoMap
  N owner <- asks (fst . _destRepo)
  traverse_
    (\(srcUser, destUser) ->
      when (owner /= destUser) $ do
        inv <- destRepo addCollaboratorR ($ (N destUser))
        destWithAuth srcUser (acceptInvitationFromR $ repoInvitationId inv)
    )
    $ H.toList nameMap

-- ============ Transfer Utils =================

-- | Lookup what each user in source is called in dest
getDestUsers :: V.Vector (Name User) -> App (V.Vector (Name User))
getDestUsers sourceAssignees = do
  userNameHt <- asks _userInfoMap
  pure $ N <$> findDestUser userNameHt (untagName <$> sourceAssignees)
  where
    findDestUser nameHt = V.mapMaybe (`H.lookup` nameHt)

transferIssues :: App ()
transferIssues = do
  vec <- sourceRepo issuesForRepoR $ \f -> f (stateAll<>sortAscending) FetchAll
  traverse_ transferIssue vec
  where
    transferIssue :: Issue -> App ()
    transferIssue iss = do
      let (N authorName) = simpleUserLogin . issueUser $ iss
          srcAssignees = simpleUserLogin <$> issueAssignees iss
      destAssignees <- getDestUsers srcAssignees
      _ <- destRepoWithAuth authorName createIssueR ($ NewIssue
          { newIssueTitle     = issueTitle iss
          , newIssueBody      = (<> ("\n\n_Original Author: " <> authorName <> "_\n\n_(Moved with "<> pkgInfo ^. _3 <> ")_")) <$> issueBody iss
          , newIssueLabels    = Just (labelName <$> issueLabels iss)
          , newIssueAssignees = if V.null (issueAssignees iss)
                                  then mempty
                                  else destAssignees
          , newIssueMilestone = milestoneNumber <$> issueMilestone iss
          })
      transferIssueComments iss
      maybeCloseIssue iss

-- | If the issue is closed in src, it closes it in dest
maybeCloseIssue :: Issue -> App ()
maybeCloseIssue iss =
  for_ (issueClosedAt iss) $ \_ -> do
    let (N authorName) = simpleUserLogin . issueUser $ iss
        iid = Id $ unIssueNumber $ issueNumber iss
    destRepoWithAuth authorName editIssueR $ \f -> f iid editOfIssue{ editIssueState = Just StateClosed }

transferIssueComments :: Issue -> App ()
transferIssueComments iss = do
  let iid = issueNumber iss
  cmnts <- sourceRepo commentsR $ \f -> f iid FetchAll
  traverse_ (transferSingleComment iid) cmnts
  where
    transferSingleComment :: IssueNumber -> IssueComment -> App Comment
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
  sourceMilestones <- sourceRepo milestonesWithOptsR $ \f -> f (stateAll <> sortAscending) FetchAll
  deletedMlstns <- transferAllMilestones 1 (sortOn milestoneNumber $ V.toList sourceMilestones) []
  liftIO $ print $ "Deleting the following milestones in dest: " <> show (map (\(Id n) -> n) deletedMlstns)
  traverse_ (\mid -> destRepo deleteMilestoneR ($ mid)) deletedMlstns
  where
    dummyNewMilestone :: NewMilestone
    dummyNewMilestone =
      NewMilestone
        { newMilestoneTitle = "Dummy milestone"
        , newMilestoneState = "closed"
        , newMilestoneDescription = Nothing
        , newMilestoneDueOn = Nothing
        }
    -- Plugs in the deleted milestones and transfers them as well.
    -- Returns a list of milestone numbers that need to be deleted in dest
    transferAllMilestones :: Int -> [Milestone] -> [Id Milestone] -> App [Id Milestone]
    transferAllMilestones _ [] acc = pure acc
    transferAllMilestones n mlist@(m:ms) acc =
      let (Id mid) = milestoneNumber m in
      if mid == n then do
        _ <- transferSingleMilestone m
        transferAllMilestones (n + 1) ms acc
      else do
        liftIO $ print $ "Milestone " <> show n <> " appears to have been deleted in source."
        toBeDeleted <- destRepo createMilestoneR ($ dummyNewMilestone {newMilestoneTitle = "Dummy milestone: " <> (T.pack . show $ n)})
        transferAllMilestones (n + 1) mlist ((milestoneNumber toBeDeleted):acc)
    milestoneToNewMilestone :: Milestone -> NewMilestone
    milestoneToNewMilestone mlstn =
      NewMilestone
        { newMilestoneTitle = milestoneTitle mlstn
        , newMilestoneState = milestoneState mlstn
        , newMilestoneDescription = milestoneDescription mlstn
        , newMilestoneDueOn = milestoneDueOn mlstn
        }
    transferSingleMilestone :: Milestone -> App Milestone
    transferSingleMilestone mlstn =
      let (N authorName) = simpleUserLogin . milestoneCreator $ mlstn in
      destRepoWithAuth authorName createMilestoneR ($ milestoneToNewMilestone mlstn)

milestonesWithOptsR :: Name Owner -> Name Repo -> IssueRepoMod -> FetchCount -> Request k (V.Vector Milestone)
milestonesWithOptsR user repo opts =
    pagedQuery ["repos", toPathPart user, toPathPart repo, "milestones"] optsStr
    where
      optsStr = issueRepoModToQueryString opts
