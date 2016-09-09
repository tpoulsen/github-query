{-# LANGUAGE OverloadedStrings #-}

module Query where

import Types

import Control.Lens             ((^.), (.~), (&))
import Data.Monoid              ((<>))
import qualified Data.Text as T (Text, intercalate, unpack, words)
import Network.Wreq

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

repositories :: T.Text
repositories = "repositories?q="

githubSearchUrl :: T.Text
githubSearchUrl = "https://api.github.com/search/"

githubSearch :: T.Text -> IO GithubResponse
githubSearch q = do
  let url = T.unpack $ githubSearchUrl <> q
  let p   = defaults
  fmap (^. responseBody) . asJSON =<< getWith p url
