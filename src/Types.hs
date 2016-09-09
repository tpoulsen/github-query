{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell   #-}

module Types where

import Control.Applicative ((<$>), (<*>))
import Control.Monad       (mzero)
import Control.Lens
import Data.Aeson
import Data.Monoid
import GHC.Generics
import qualified Data.ByteString.Lazy.Internal as BS
import qualified Data.Text as T

type Url = Maybe T.Text

data User = User
    {  _login :: T.Text
    ,  _userId :: Integer
    , _avatarUrl :: Url
    , _gravatarId :: T.Text
    , _userUrl :: Url
    , _userHtmlUrl :: Url
    , _userFollowersUrl :: Url
    , _userFollowingUrl :: Url
    , _gistsUrl :: Url
    , _starredUrl :: Url
    , _subscriptionsUrl :: Url
    , _organizationsUrl :: Url
    , _reposUrl :: Url
    , _eventsUrl :: Url
    , _receivedEventsUrl :: Url
    , _userType :: T.Text
    , _siteAdmin :: Bool
    } deriving (Show, Eq, Generic)

makeLenses ''User

instance FromJSON User where
    parseJSON (Object o) =
        User <$>
                o .: "login" <*>
                o .: "id" <*>
                o .: "avatar_url" <*>
                o .: "gravatar_id" <*>
                o .: "url" <*>
                o .: "html_url" <*>
                o .: "followers_url" <*>
                o .: "following_url" <*>
                o .: "gists_url" <*>
                o .: "starred_url" <*>
                o .: "subscriptions_url" <*>
                o .: "organizations_url" <*>
                o .: "repos_url" <*>
                o .: "events_url" <*>
                o .: "received_events_url" <*>
                o .: "type" <*>
                o .: "site_admin"
    parseJSON _ = mzero
instance ToJSON User where

-----------------------------------------------------------
data Repository = Repository
    {  _repositoryId :: Integer
    ,  _name :: T.Text
    , _fullName :: T.Text
    , _owner :: User
    , _private :: Bool
    , _htmlUrl :: Url
    , _description :: T.Text
    , _fork :: Bool
    , _repositoryUrl :: Url
    , _forksUrl :: Url
    , _keysUrl :: Url
    , _collaboratorsUrl :: Url
    , _teamsUrl :: Url
    , _hooksUrl :: Url
    , _issueEventsUrl :: Url
    , _repositoryEventsUrl :: Url
    , _assigneesUrl :: Url
    , _branchesUrl :: Url
    , _tagsUrl :: Url
    , _blobsUrl :: Url
    , _gitTagsUrl :: Url
    , _gitRefsUrl :: Url
    , _treesUrl :: Url
    , _statusesUrl :: Url
    , _languagesUrl :: Url
    , _stargazersUrl :: Url
    , _contributorsUrl :: Url
    , _subscribersUrl :: Url
    , _subscriptionUrl :: Url
    , _commitsUrl :: Url
    , _gitCommitsUrl :: Url
    , _commentsUrl :: Url
    , _issueCommentUrl :: Url
    , _contentsUrl :: Url
    , _compareUrl :: Url
    , _mergesUrl :: Url
    , _archiveUrl :: Url
    , _downloadsUrl :: Url
    , _issuesUrl :: Url
    , _pullsUrl :: Url
    , _milestonesUrl :: Url
    , _notificationsUrl :: Url
    , _labelsUrl :: Url
    , _releasesUrl :: Url
    , _deploymentsUrl :: Url
    , _createdAt :: T.Text
    , _updatedAt :: T.Text
    , _pushedAt :: T.Text
    , _gitUrl :: Url
    , _sshUrl :: Url
    , _cloneUrl :: Url
    , _svnUrl :: Url
    , _homepage :: Url
    , _size :: Integer
    , _stargazersCount :: Integer
    , _watchersCount :: Integer
    , _language :: Maybe T.Text
    , _hasIssues :: Bool
    , _hasDownloads :: Bool
    , _hasWiki :: Bool
    , _hasPages :: Bool
    , _forksCount :: Integer
    , _mirrorUrl :: Url
    , _openIssuesCount :: Integer
    , _forks :: Integer
    , _openIssues :: Integer
    , _watchers :: Integer
    , _defaultBranch :: T.Text
    , _score :: Double
    } deriving (Show, Eq, Generic)

makeLenses ''Repository

instance FromJSON Repository where
    parseJSON (Object o) =
        Repository <$>
                o .: "id" <*>
                o .: "name" <*>
                o .: "full_name" <*>
                o .: "owner" <*>
                o .: "private" <*>
                o .: "html_url" <*>
                o .: "description" <*>
                o .: "fork" <*>
                o .: "url" <*>
                o .: "forks_url" <*>
                o .: "keys_url" <*>
                o .: "collaborators_url" <*>
                o .: "teams_url" <*>
                o .: "hooks_url" <*>
                o .: "issue_events_url" <*>
                o .: "events_url" <*>
                o .: "assignees_url" <*>
                o .: "branches_url" <*>
                o .: "tags_url" <*>
                o .: "blobs_url" <*>
                o .: "git_tags_url" <*>
                o .: "git_refs_url" <*>
                o .: "trees_url" <*>
                o .: "statuses_url" <*>
                o .: "languages_url" <*>
                o .: "stargazers_url" <*>
                o .: "contributors_url" <*>
                o .: "subscribers_url" <*>
                o .: "subscription_url" <*>
                o .: "commits_url" <*>
                o .: "git_commits_url" <*>
                o .: "comments_url" <*>
                o .: "issue_comment_url" <*>
                o .: "contents_url" <*>
                o .: "compare_url" <*>
                o .: "merges_url" <*>
                o .: "archive_url" <*>
                o .: "downloads_url" <*>
                o .: "issues_url" <*>
                o .: "pulls_url" <*>
                o .: "milestones_url" <*>
                o .: "notifications_url" <*>
                o .: "labels_url" <*>
                o .: "releases_url" <*>
                o .: "deployments_url" <*>
                o .: "created_at" <*>
                o .: "updated_at" <*>
                o .: "pushed_at" <*>
                o .: "git_url" <*>
                o .: "ssh_url" <*>
                o .: "clone_url" <*>
                o .: "svn_url" <*>
                o .: "homepage" <*>
                o .: "size" <*>
                o .: "stargazers_count" <*>
                o .: "watchers_count" <*>
                o .: "language" <*>
                o .: "has_issues" <*>
                o .: "has_downloads" <*>
                o .: "has_wiki" <*>
                o .: "has_pages" <*>
                o .: "forks_count" <*>
                o .: "mirror_url" <*>
                o .: "open_issues_count" <*>
                o .: "forks" <*>
                o .: "open_issues" <*>
                o .: "watchers" <*>
                o .: "default_branch" <*>
                o .: "score"
    parseJSON _ = mzero
instance ToJSON Repository where

-----------------------------------------------------------
data GithubResponse = GithubResponse
    {  _resultCount :: Integer
    ,  _incompleteResults :: Bool
    ,  _items :: [Repository]
    } deriving (Show, Eq, Generic)

makeLenses ''GithubResponse

instance FromJSON GithubResponse where
    parseJSON (Object o) =
        GithubResponse <$>
                o .: "total_count" <*>
                o .: "incomplete_results" <*>
                o .: "items"
    parseJSON _  = mzero
instance ToJSON GithubResponse where
