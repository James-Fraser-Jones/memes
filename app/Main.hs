{-
Webdriver Stackage Docs: https://www.stackage.org/package/webdriver
Hello World Example: https://github.com/kallisti-dev/hs-webdriver/blob/master/examples/readme-example-beginner.md
Selenium Standalone Server: https://docs.seleniumhq.org/download/ (requires JRE)
Chrome ChromeDriver: https://sites.google.com/a/chromium.org/chromedriver/ (make sure it's on your PATH, I put it in "/usr/local/bin")
Added "webdriver" to package.yaml executable dependencies

Firefox Geckodriver: https://github.com/mozilla/geckodriver/releases (make sure it's on your PATH)
Recent issue with Firefox: https://github.com/kallisti-dev/hs-webdriver/issues/136
-}

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Control.Monad.IO.Class
import qualified Data.Text as T
import Test.WebDriver
import Text.HTML.Scalpel.Core
import qualified Text.HTML.Scalpel.Core as S

main :: IO ()
main = do
  pageHTML <- getFresh
  print $ maybe "Error" show $ scrapeStringLike pageHTML feedScraper
  pure ()

--------------------------------------------------------------------------------

chromeConfig :: WDConfig
chromeConfig = useBrowser chrome defaultConfig

getFresh :: IO T.Text
getFresh = runSession chromeConfig $ openPage "https://9gag.com/fresh" *> getSource

--------------------------------------------------------------------------------

feedSelector :: S.Selector
feedSelector = TagString "div" @: ["id" @= "list-view-2"]

feedScraper :: Scraper T.Text [T.Text]
feedScraper = chroot feedSelector $ attrs "id" $ tagSelector "article"
