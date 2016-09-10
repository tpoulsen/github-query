{-# LANGUAGE OverloadedStrings #-}

module Github where

import Github.Api.Query
import Github.Api.Types

import Control.Lens ((^.))
import qualified Data.Text as T (Text)


-- Convenience functions for different query types
repositorySearch :: T.Text -> IO GithubResponse
repositorySearch = githubSearch Repositories

userSearch :: T.Text -> IO GithubResponse
userSearch = githubSearch Users

totalResults :: GithubResponse -> Integer
totalResults response = response ^. resultCount

allResults :: GithubResponse -> [ApiItem]
allResults response = response ^. items

