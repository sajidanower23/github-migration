{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import GitHub

import Configuration.Utils
import Data.Aeson
import Data.Text                (Text)
import PkgInfo_github_migration

import Lens.Micro.TH


data Config = Config
  { _fromURL    :: Text
  , _toURL      :: Text
  , _fromAPIKey :: Text
  } deriving Show
$(makeLenses ''Config)

defaultConfig :: Config
defaultConfig = Config
  {_fromURL = "https://github.csiro.au/api/v3"
  ,_toURL= "https://api.github.com/api/v3"
  ,_fromAPIKey=""}


instance FromJSON (Config -> Config) where
  parseJSON = withObject "Config" $ \o -> id
    <$< fromURL    ..: "from-url"     % o
    <*< toURL      ..: "to-url"       % o
    <*< fromAPIKey ..: "from-api-key" % o

instance ToJSON Config where
  toJSON (Config furl turl fkey) = object
    [ "from-url" .= furl
    , "to-url" .= turl
    , "from-api-key" .= fkey
    ]

flg :: HasName f => Char -> String -> String -> Mod f a
flg c l h = long l <> short c <> help h

pConfig :: MParser Config
pConfig = id
  <$< fromURL    .:: strOption % flg 'f' "from-url"     "From URL"
  <*< toURL      .:: strOption % flg 't' "to-url"       "to URL"
  <*< fromAPIKey .:: strOption % flg 'k' "from-api-key" "From API Key"

mainInfo :: ProgramInfo Config
mainInfo = programInfo "Hello World" pConfig defaultConfig

main :: IO ()
main = runWithPkgInfoConfiguration mainInfo pkgInfo $ \conf -> do
  putStrLn "hello world"
  print conf
