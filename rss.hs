{-# LANGUAGE FlexibleContexts #-}
module Main where

import Text.XML.Light
import Text.Feed.Import
import Text.Feed.Export
import System.Environment
import Database.HDBC
import Database.HDBC.Sqlite3 (connectSqlite3)
import Data.Convertible.Base

-- Data
data RSSItem = RSSItem { it_title       :: Maybe String
                       , it_url         :: Maybe String
                       , it_feed_url    :: Maybe String
                       , it_description :: Maybe String
                       , it_author      :: Maybe String
                       , it_guid        :: Maybe String
                       , it_pubDate     :: Maybe Integer
                       , it_enc_url     :: Maybe String
                       , it_enc_type    :: Maybe String
                       , it_id          :: Maybe Integer
                       , it_unread      :: Maybe Bool
                       } deriving (Show)
data RSSFeed = RSSFeed { fd_rssurl :: Maybe String
                       , fd_url    :: Maybe String
                       , fd_title  :: Maybe String
                       } deriving (Show)

-- Database Querying
loadFeeds :: (IConnection c) => c -> IO [RSSFeed]
loadFeeds conn = (fmap . map) rowToFeed $ quickQuery' conn "SELECT rssurl,url,title FROM rss_feed" []
    where rowToFeed :: [SqlValue] -> RSSFeed
          rowToFeed (r:u:t:[]) = RSSFeed (Just $ fromSql r) (Just $ fromSql u) (Just $ fromSql t)

loadFeedItems :: (IConnection c) => c -> RSSFeed -> IO [RSSItem]
loadFeedItems conn fd = (fmap . map) rowToItem $ quickQuery' conn "SELECT title,url,feedurl,author,guid,pubDate,enclosure_url,enclosure_type,id,unread FROM rss_item WHERE feedurl = ?" [url]
    where url = toSql $ fd_rssurl fd
          rowToItem :: [SqlValue] -> RSSItem
          rowToItem (t:u:f:a:g:p:eu:et:id:ur:[]) = RSSItem (mfsql t)  (mfsql u)  (mfsql f)
                                                           Nothing    (mfsql a)  (mfsql g)
                                                           (mfsql p)  (mfsql eu) (mfsql et)
                                                           (mfsql id) (bfsql ur)
              where mfsql :: (Convertible SqlValue a) => SqlValue -> Maybe a
                    mfsql v = Just $ fromSql v
                    bfsql :: SqlValue -> Maybe Bool
                    bfsql v = Just True -- TODO read

-- Database writing (caller must call commit itself)
addFeed :: (IConnection c) => c -> RSSFeed -> IO ()
addFeed conn fd = do insert_if conn r
                     updateFeed conn fd
    where r = fd_rssurl fd
          insert_if c (Just r) = do run c "INSERT INTO rss_feed (rssurl,url,title) VALUES (?,'','')" [toSql r]
                                    return ()
          insert_if _ Nothing = return()

updateFeed :: (IConnection c) => c -> RSSFeed -> IO()
updateFeed conn fd = do update_if conn r u "url"
                        update_if conn r t "title"
    where r = fd_rssurl fd
          u = fd_url fd
          t = fd_title fd
          update_if _ Nothing _ _ = return()
          update_if _ _ Nothing _ = return()
          update_if c (Just r) (Just v) s = do run c ("UPDATE rss_feed SET " ++ s ++ " = ? WHERE rssurl = ?") [toSql v, toSql r]
                                               return()

