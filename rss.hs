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
loadFeedItems _ (RSSFeed Nothing _ _) = return []
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
                    bfsql v = Just $ intToBool $ fromSql v
                        where intToBool :: Int -> Bool
                              intToBool 0 = False
                              intToBool _ = True

-- Database writing (caller must call commit itself)
addFeed :: (IConnection c) => c -> RSSFeed -> IO ()
addFeed conn fd = do insert_if conn r
                     updateFeed conn fd
    where r = fd_rssurl fd
          insert_if c (Just r) = do run c "INSERT INTO rss_feed (rssurl,url,title) VALUES (?,'','')" [toSql r]
                                    return ()
          insert_if _ Nothing = return()

updateFeed :: (IConnection c) => c -> RSSFeed -> IO()
updateFeed _ (RSSFeed Nothing _ _) = return()
updateFeed c (RSSFeed r       u t) = do update_if c r u "url"
                                        update_if c r t "title"
    where update_if _ _ Nothing _ = return()
          update_if c (Just r) (Just v) s = do run c ("UPDATE rss_feed SET " ++ s ++ " = ? WHERE rssurl = ?") [toSql v, toSql r]
                                               return()

updateItem :: (IConnection c) => c -> RSSItem -> IO()
updateItem _ (RSSItem _ _ _ _ _ _ _ _  _  Nothing _)   = return()
updateItem c (RSSItem t u f d a g p eu et id      urd) = do update_if c t  id "title"
                                                            update_if c u  id "url"
                                                            update_if c f  id "feedurl"
                                                            update_if c d  id "description"
                                                            update_if c a  id "author"
                                                            update_if c g  id "guid"
                                                            update_if c p  id "pubDate"
                                                            update_if c eu id "enclosure_url"
                                                            update_if c et id "enclosure_type"
                                                            update_if c ur id "unread"
    where ur = bti urd
          update_if _ Nothing   _        _ = return()
          update_if c (Just v) (Just id) s = do run c ("UPDATE rss_item SET " ++ s ++ " = ? WHERE id = ?") [toSql v, toSql id]
                                                return()
          bti :: Maybe Bool -> Maybe Int
          bti Nothing      = Nothing
          bti (Just True)  = Just 1
          bti (Just False) = Just 0

