{-# LANGUAGE OverloadedStrings #-}

module Query where

import Types

import Control.Applicative      ((<$>))
import Control.Lens             ((^.), (.~), (&))
import Data.Monoid              ((<>))
import qualified Data.Text as T (Text, append, pack, unpack, concat)
import Network.Wreq

byCreatedDate :: T.Text -> T.Text -> T.Text
byCreatedDate query date =
  T.append query dateString
  where dateString = T.append "+created:" date

runQuery :: T.Text -> IO GithubResponse
runQuery query = do
  let url    = T.unpack $ T.concat ["https://api.github.com/search/repositories?q=", query]
  let params = defaults
  fmap (^. responseBody) . asJSON =<< getWith params url
