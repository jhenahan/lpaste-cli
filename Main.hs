{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import           Data.Char              (toLower)
import           Data.Maybe             (fromMaybe)
import           Network.Browser
import           Network.HTTP.Base      (Request, RequestMethod (POST))
import           Network.URI            (URI, parseURI)
import           System.Console.CmdArgs
import           System.Directory       (doesFileExist)
import           System.Environment     (getEnv)

main :: IO ()
main = do
  authorName <- getEnv "USER"
  conf <- cmdArgs $ config authorName
  let input = file conf
  exists <- doesFileExist input
  content <- if exists then readFile input else getContents
  let postFn = if test conf then testing else post
  resultUrl <- postFn conf content
  print resultUrl

data Config = Config { author   :: String
                     , language :: String
                     , title    :: String
                     , uri      :: String
                     , file     :: FilePath
                     , test     :: Bool }
              deriving (Data, Typeable)

instance Show Config where
  show conf = concat [ "Author: ", author conf, "\n"
                     , "Language: ", language conf, "\n"
                     , "Title: ", title conf, "\n"
                     , "URI: ", uri conf]

config :: String -> Config
config handle = Config { author = handle
                                &= help "Your handle (defaults to $USER)"
                                &= typ "HANDLE"
                         , language = "haskell"
                                &= help "The language used for syntax highlighting (default: haskell)"
                                &= typ "LANGUAGE"
                         , title = ""
                                &= help "The title of the snippet"
                                &= typ "TITLE"
                                &= name "t"
                         , uri = "http://lpaste.net/"
                                &= help "The URI of the lpaste instance"
                                &= typ "URL"
                         , file = ""
                                &= args
                                &= typFile
                         , test = False
                                &= help "Dry run: echo config and content"
                         }
                         &= program "lpaste-cli"
                         &= summary "lpaste-cli v1.0 Copyright (C) 2013 Jack Henahan"

newUri :: String -> URI
newUri u = buildURI u "new"

buildURI :: String -> String -> URI
buildURI u str = fromMaybe (error "Invalid URI.") $ parseURI $ u ++ str

post :: Config -> String -> IO URI
post conf str = do
  (url, _) <- Network.Browser.browse $ do
                  setOutHandler $ const (return ())
                  setAllowRedirects True
                  request $ buildRequest conf str
  return url

buildRequest :: Config -> String -> Request String
buildRequest conf str = formToRequest $ Form POST (newUri $ uri conf)
                             [ ("title", title conf)
                             , ("author", author conf)
                             , ("paste", str)
                             , ("language", map toLower $ language conf)
                             , ("channel", "")
                             , ("email", "")
                             ]

testing :: Config -> String -> IO URI
testing conf str = do
  print conf
  putStrLn $ "content: " ++ str
  return $ fromMaybe (error "Invalid URI.") $ parseURI $ uri conf
