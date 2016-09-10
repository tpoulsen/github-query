{-# LANGUAGE OverloadedStrings #-}

module Github.Api.Query where

import Github.Api.Types

import Control.Lens             ((^.), (.~), (&), (^?))
import Data.Monoid              ((<>))
import qualified Data.Text as T (Text, intercalate, unpack, words)
import Network.Wreq

data SearchType = Repositories | Users | Code | Issues deriving (Show, Eq, Ord)
data SortOrder = Asc | Desc deriving (Show, Eq, Ord)
data SortBy = Stars | Forks | Updated | Match deriving (Show, Eq, Ord)

byCreatedDate :: T.Text -> T.Text
byCreatedDate date = "+created:" <> date

byUser :: T.Text -> T.Text
byUser username = "+user:" <> username

byLanguage :: T.Text -> T.Text
byLanguage lang = "+language:" <> lang

sort :: SortBy -> T.Text
sort s =
  "&sort=" <> sortBy s
  where sortBy s'
          | s' == Stars = "stars"
          | s' == Forks = "forks"
          | s' == Updated = "updated"
          | otherwise = "match"

order :: SortOrder -> T.Text
order o =
  "&order=" <> orderBy o
  where orderBy o'
          | o' == Asc  = "asc"
          | otherwise = "desc"

query :: T.Text -> T.Text
query = T.intercalate "%20" . T.words

searchBy :: SearchType -> T.Text
searchBy t =
  searchType t <> "?q="
  where searchType t'
          | t' == Repositories = "repositories"
          | t' == Users = "users"
          | t' == Code = "code"
          | t' == Issues = "issues"
          | otherwise = "repositories"

githubSearchUrl :: T.Text
githubSearchUrl = "https://api.github.com/search/"

githubSearch :: SearchType -> T.Text -> IO GithubResponse
githubSearch t q = do
  let url = T.unpack $ githubSearchUrl <> (searchBy t) <> q
  let p   = defaults
  fmap (^. responseBody) . asJSON =<< getWith p url
