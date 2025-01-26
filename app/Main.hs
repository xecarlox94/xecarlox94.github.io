module Main where

import Hakyll

-- import System
import System.FilePath

import Data.List

main :: IO ()
main = hakyllWith config do

config :: Configuration
config = defaultConfiguration
    { destinationDirectory  = "docs"
    , previewPort           = 5000
    , providerDirectory     = "src"
    }

-- https://robertwpearce.com/hakyll-pt-1-setup-initial-customization.html

{-
-- | Default configuration for a hakyll application
defaultConfiguration :: Configuration
defaultConfiguration = Configuration
    { destinationDirectory = "_site"
    , storeDirectory       = "_cache"
    , tmpDirectory         = "_cache/tmp"
    , providerDirectory    = "."
    , ignoreFile           = ignoreFile'
    , deployCommand        = ""
    , deploySite           = \_ -> error ""
    , inMemoryCache        = True
    , previewHost          = "127.0.0.1"
    , previewPort          = 8000
    }

  where

  ignoreFile' path
      | "."    `isPrefixOf` fileName = True
      | "#"    `isPrefixOf` fileName = True
      | "~"    `isSuffixOf` fileName = True
      | ".swp" `isSuffixOf` fileName = True
      | otherwise                    = False

    where

    fileName = takeFileName path
-}
