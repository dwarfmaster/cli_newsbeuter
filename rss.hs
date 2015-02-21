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
data FDType  = Plain | Filter | Exec
data RSSFeed = RSSFeed { fd_rssurl :: Maybe String
                       , fd_url    :: Maybe String
                       , fd_title  :: Maybe String
                       , fd_type   :: Maybe FDType
                       , fd_tags   :: Maybe [String]
                       } deriving (Show)

-- Data classes
showFDType :: FDType -> String
showFDType Plain  = "Plain"
showFDType Filter = "Filter"
showFDType Exec   = "Exec"
instance Show FDType where
    show = showFDType

-- Database Querying
rowToFeed :: [SqlValue] -> RSSFeed
rowToFeed (r:u:t:[]) = RSSFeed (Just $ fromSql r) (Just $ fromSql u) (Just $ fromSql t) Nothing Nothing
rowToFeed _          = RSSFeed Nothing Nothing Nothing Nothing Nothing

loadFeeds :: (IConnection c) => c -> IO [RSSFeed]
loadFeeds conn = (fmap . map) rowToFeed $ quickQuery' conn "SELECT rssurl,url,title FROM rss_feed" []

hasFeed :: (IConnection c) => c -> RSSFeed -> IO Bool
hasFeed _    (RSSFeed Nothing _ _ _ _) = return False
hasFeed conn (RSSFeed rssurl  _ _ _ _) = do lst <- quickQuery' conn "SELECT rssurl FROM rss_feed WHERE rssurl = ?" [toSql rssurl]
                                            return (length lst /= 0)

populateFeed :: (IConnection c) => c -> RSSFeed -> IO RSSFeed
populateFeed conn fd = fmg $ frowToFeed $ fhead $ quickQuery' conn "SELECT rssurl,url,title FROM rss_feed WHERE rssurl = ?" [rurl]
    where rurl       = toSql $ fd_rssurl fd
          fhead      = fmap head
          frowToFeed = fmap rowToFeed
          fmg        = fmap (mergeTwo fd)
          mergeTwo :: RSSFeed -> RSSFeed -> RSSFeed
          mergeTwo (RSSFeed rss _ _ _ tgs) (RSSFeed _ u t _ _) = RSSFeed rss u t Nothing tgs

loadFeedItems :: (IConnection c) => c -> RSSFeed -> IO [RSSItem]
loadFeedItems _ (RSSFeed Nothing _ _ _ _) = return []
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
updateFeed _ (RSSFeed Nothing _ _ _ _) = return()
updateFeed c (RSSFeed r       u t _ _) = do update_if c r u "url"
                                            update_if c r t "title"
    where update_if _ _ Nothing _ = return()
          update_if c (Just r) (Just v) s = do run c ("UPDATE rss_feed SET " ++ s ++ " = ? WHERE rssurl = ?") [toSql v, toSql r]
                                               return()

addItem :: (IConnection c) => c -> RSSItem -> IO()
addItem conn it = do x <- quickQuery' conn "SELECT MAX(id) FROM rss_item" []
                     run conn "INSERT INTO rss_item (guid,title,author,url,feedurl,pubDate,content,unread,enclosure_url,enclosure_type) VALUES ('','','','','',0,'',1,'','')" []
                     updateItem conn $ setId it $ (pop x) + 1
    where pop :: [[SqlValue]] -> Integer
          pop x = fromSql $ head $ head x
          setId (RSSItem t u f d a g p eu et _ ur) id = RSSItem t u f d a g p eu et (Just id) ur

updateItem :: (IConnection c) => c -> RSSItem -> IO()
updateItem _ (RSSItem _ _ _ _ _ _ _ _  _  Nothing _)   = return()
updateItem c (RSSItem t u f d a g p eu et id      urd) = do update_if c t  id "title"
                                                            update_if c u  id "url"
                                                            update_if c f  id "feedurl"
                                                            update_if c d  id "content"
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

-- Load RSSFeeds from url file
cutLine :: String -> String -> Bool -> [String]
cutLine ""       ""    _     = []
cutLine ""       chunk _     = [chunk]
cutLine ('"':ls) ""    True  = cutLine ls "" False
cutLine ('"':ls) chunk True  = chunk : cutLine ls "" False
cutLine (l:ls)   chunk True  = cutLine ls (chunk ++ [l]) True
cutLine ('"':ls) ""    False = cutLine ls "" True
cutLine ('"':ls) chunk False = chunk:cutLine ls "" True
cutLine (l:ls)   chunk False = if isBlank l then if chunk == "" then cutLine ls "" False
                                                 else chunk : cutLine ls "" False
                               else cutLine ls (chunk ++ [l]) False
    where isBlank ' '  = True
          isBlank '\t' = True
          isBlank '\n' = True
          isBlank  _   = False

loadFeedsFromFile :: FilePath -> IO [RSSFeed]
loadFeedsFromFile ""   = return []
loadFeedsFromFile path = fmap rmNothing $ (fmap.map) parseLine $ (fmap lines . readFile) path
    where rmNothing :: [Maybe RSSFeed] -> [RSSFeed]
          rmNothing  []           = []
          rmNothing (Nothing:rs)  = rmNothing rs
          rmNothing ((Just r):rs) = r:rmNothing rs
          parseLine :: String -> Maybe RSSFeed
          parseLine str = if length parts == 0 then Nothing
                          else Just $ RSSFeed (Just $ head parts) Nothing Nothing Nothing (Just $ tail parts)
              where parts = cutLine str "" False

