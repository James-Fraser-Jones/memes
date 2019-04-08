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
import Test.WebDriver.Commands.Wait
import Text.HTML.Scalpel.Core
import qualified Text.HTML.Scalpel.Core as S
import Data.Time.Clock

--import qualified Text.HTML.TagSoup as TagSoup

data GagPost = Post {gp_id :: T.Text, gp_time :: UTCTime, gp_title :: T.Text, gp_tags :: [T.Text]}

main :: IO ()
main = runSession (useBrowser chrome defaultConfig) processFresh

headless :: WDConfig -> WDConfig
headless config = config {wdCapabilities = headles $ wdCapabilities config}
  where headles capabilities = capabilities {browser = headle $ browser capabilities}
        headle browser = browser {chromeOptions = "headless" : chromeOptions browser}

processFresh :: (WebDriver wd, MonadIO wd) => wd ()
processFresh = do
  openPage "https://www.9gag.com/fresh"
  closePopupButton <- findElem $ ByClass "closebutton_closeButton--3abym"
  click closePopupButton
  -- freshSource <- getSource
  -- let articleSearch = scrapeStringLike freshSource $ attrs "id" $ tagSelector "article"
  -- maybe searchError processArticles articleSearch
  --   where searchError = liftIO $ print "Error: No Articles Found"
  --         processArticles = traverse_ processArticle . fmap (T.append "https://www.9gag.com/gag/" . T.drop 10) . filter (/= "")

processArticle :: (WebDriver wd, MonadIO wd) => T.Text -> wd ()
processArticle url = do
  openPage $ T.unpack url
  articleSource <- getSource
  let infoSearch = scrapeStringLike articleSource articleScraper
  pure ()

articleScraper :: Scraper T.Text GagPost
articleScraper = undefined

safeRead :: Read a => T.Text -> Maybe a
safeRead = readMaybe . T.unpack

{-------------------------------------------------------------------------------
Algorithm:
1. Open fresh.
2. Scroll as far as necessary to load articles up to ones previously seen, or a maximum, whichever is smaller.
3. Collect ids for all of these
4. traverse article processing function
5. Make sure to use safe read when creating Gagposts.
6. Add to database
7. Run every x seconds. (60?)
8.
-}

{-------------------------------------------------------------------------------
Code Graveyard:

data ArticleInfo = ArticleInfo {title :: T.Text, tags :: [T.Text], time :: Int, points :: Int, comments :: Int} deriving Show

readComments :: T.Text -> Int
readComments text = maybe (-1) id $ safeRead firstWord
  where firstWord = T.filter (/= ',') $ head $ T.words text

readPoints :: T.Text -> Int
readPoints text
  | T.head firstWord == '\8226' = 0
  | otherwise = maybe (-1) id $ safeRead firstWord
  where firstWord = T.filter (/= ',') $ head $ T.words text

articleScraper :: Scraper T.Text ArticleInfo
articleScraper = ArticleInfo <$>
                (text $ article // ("h1" @: [])) <*>
                (pure []) <*>
                (pure 0) <*>
                fmap readPoints (text $ article // ("a" @: ["class" @= "point badge-evt"])) <*>
                fmap readComments (text $ article // ("a" @: ["class" @= "comment badge-evt"]))
  where article = ("article" @: [])
-}
