{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List (isPrefixOf, isSuffixOf, sortBy)
import qualified Data.Text as T
import System.Environment (getArgs)
import System.IO ()
import Text.Parsec (
  ParseError,
  anyChar,
  char,
  eof,
  manyTill,
  newline,
  parse,
  string,
  try,
  (<|>),
 )
import Text.Parsec.String (GenParser)


main :: IO ()
main = do
  args <- getArgs
  case args of
    [old_url, new_engine] -> qutesearch old_url new_engine
    _ -> putStrLn "Wrong number of arguments"


-- | sortBy is used to disambiguate multiple matches. Otherwise the (very common) empty postfix string 
--  always matches (and gets selected) when we are actually looking for a longer postfix string.
-- Example: 
-- c.url.searchengines["gl"] =  "https://www.google.com/search?q={}"
-- c.url.searchengines["gli"] = "https://www.google.com/search?q={}&tbm=isch"
-- Without sortBy, both "gl" and "gli" would match and the first one ("gl") would be selected 
-- (with `head`) when calling qutebrowser with urls of type "gli". This causes the next search to carry "&tbm=isch" as part of the search string.
-- Disambiguation has not been tested thoroughly, there could be some other uncaught problematic cases. In regular practice it worked very well.
qutesearch :: String -> String -> IO ()
qutesearch old_url new_engine = do
  file <- readFile "/home/fcortesi/.config/qutebrowser/config.py"
  case parseEngines file of
    Left x -> print x
    Right engines -> do
      let all_engines = mconcat engines
          (_engine, url_start, url_end) = head $ sortBy (\(_, _, a) (_, _, b) -> if length a > length b then LT else GT) (filter (\(engine, url_start, url_end) -> isPrefixOf url_start old_url && isSuffixOf url_end old_url) all_engines)
          url_minus_start = T.replace (T.pack url_start) "" (T.pack old_url)
          search_string = if null url_end then url_minus_start else T.replace (T.pack url_end) "" url_minus_start
          (_, new_url_start, new_url_end) = head $ filter (\(engine, _, _) -> engine == new_engine) all_engines
          new_url = new_url_start <> T.unpack search_string <> new_url_end
      putStrLn new_url

parseEngines :: String -> Either ParseError [[(String, String, String)]]
parseEngines = parse (manyTill (try (do
                                      string "c.url.searchengines[\""
                                      engine <- manyTill anyChar $ char '"'
                                      manyTill anyChar $ char '"'
                                      url_start <- manyTill anyChar $ char '{'
                                      manyTill anyChar $ char '}'
                                      url_end <- manyTill anyChar $ char '"'
                                      pure [(engine, url_start, url_end)]
                                  ) <|> (manyTill anyChar newline >> pure [])) eof) "(unknown)"
