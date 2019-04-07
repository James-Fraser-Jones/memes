{-
Scalpel-Core Stackage Docs: https://www.stackage.org/package/scalpel-core
Webdriver Stackage Docs: https://www.stackage.org/package/webdriver
Hello World WebDriver Example: https://github.com/kallisti-dev/hs-webdriver/blob/master/examples/readme-example-beginner.md
Selenium Standalone Server: https://docs.seleniumhq.org/download/ (requires JRE)
Chrome ChromeDriver: https://sites.google.com/a/chromium.org/chromedriver/ (make sure it's on your PATH, I put it in "/usr/local/bin")

Startup Server: java -jar selenium-server-standalone-*.jar

Firefox Geckodriver: https://github.com/mozilla/geckodriver/releases (make sure it's on your PATH)
Recent issue with Firefox: https://github.com/kallisti-dev/hs-webdriver/issues/136
-}

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Data.Foldable
import Control.Monad.IO.Class
import Text.Read
import qualified Data.Text as T
import Test.WebDriver
import Test.WebDriver.Class
import Text.HTML.Scalpel.Core
import qualified Text.HTML.Scalpel.Core as S

--import qualified Text.HTML.TagSoup as TagSoup

data ArticleInfo = ArticleInfo {title :: T.Text, tags :: [T.Text], time :: Int, points :: Int, comments :: Int} deriving Show

main :: IO ()
main = runSession (useBrowser chrome defaultConfig) processFresh

processFresh :: (WebDriver wd, MonadIO wd) => wd ()
processFresh = do
  openPage "https://www.9gag.com/hot"
  freshSource <- getSource
  let articleSearch = scrapeStringLike freshSource $ attrs "id" $ tagSelector "article"
  maybe searchError processArticles articleSearch
    where searchError = liftIO $ print "Error: No Articles Found"
          processArticles = traverse_ processArticle . fmap (T.append "https://www.9gag.com/gag/" . T.drop 10) . filter (/= "")

processArticle :: (WebDriver wd, MonadIO wd) => T.Text -> wd ()
processArticle url = do
  openPage $ T.unpack url
  articleSource <- getSource
  let infoSearch = scrapeStringLike articleSource articleScraper
  liftIO $ print $ maybe "Error: No Info Found" show infoSearch

articleScraper :: Scraper T.Text ArticleInfo
articleScraper = ArticleInfo <$>
                (text $ article // ("h1" @: [])) <*>
                (pure []) <*>
                (pure 0) <*>
                fmap readPoints (text $ article // ("a" @: ["class" @= "point badge-evt"])) <*>
                fmap readComments (text $ article // ("a" @: ["class" @= "comment badge-evt"]))
  where article = ("article" @: [])

--------------------------------------------------------------------------------

readComments :: T.Text -> Int
readComments text = maybe (-1) id $ safeRead firstWord
  where firstWord = T.filter (/= ',') $ head $ T.words text

readPoints :: T.Text -> Int
readPoints text
  | T.head firstWord == '\8226' = 0
  | otherwise = maybe (-1) id $ safeRead firstWord
  where firstWord = T.filter (/= ',') $ head $ T.words text

safeRead :: Read a => T.Text -> Maybe a
safeRead = readMaybe . T.unpack
