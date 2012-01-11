module Main where

import Data.Maybe
import Network.HTTP
import Text.Atom.Feed
import Text.Feed.Query
import Text.Feed.Constructor
import Text.Feed.Types
import Text.Feed.Import
import Text.XML.Light
import System.IO
import System.Cmd

{- Example item:
(22,RSSItem (RSSItem {rssItemTitle = Just "Zerker vs Spades (1 Game)", rssItemLink = Just "http://sc2casts.com/cast7028-Zerker-vs-Spades-1-Game-Starcraft-Ladder-Battle.net-VOD", rssItemDescription = Just "[ZvT] Starcraft Ladder - Battle.net VOD / cast by: NanMan @ YouTube", rssItemAuthor = Nothing, rssItemCategories = [], rssItemComments = Nothing, rssItemEnclosure = Nothing, rssItemGuid = Just (RSSGuid {rssGuidPermanentURL = Nothing, rssGuidAttrs = [], rssGuidValue = "7028"}), rssItemPubDate = Just "Mon, 09 Jan 2012 00:00:00 +0100", rssItemSource = Nothing, rssItemAttrs = [], rssItemOther = []}))
-}

main :: IO ()
main = do
  putStrLn "Starting Feed"
  rsp <- Network.HTTP.simpleHTTP (getRequest "http://sc2casts.com/rss")
  body <- getResponseBody rsp
  -- ok to here
  let feed = parseFeedString body
  let items = maybe ([newItem . RSSKind . Just $ "fail"]) feedItems feed
  selections <- selectFromItems items
  putStrLn $ "Fetching: " ++ (show . length $ selections) ++ " Items"
  _ <- fetchItems selections
  return ()

-- selectFromItems :: [Items] -> IO [Items]
selectFromItems items = do
  let enumItems = zip [1..] items
  displayItems enumItems
  putStr "Fetch: "
  hFlush stdout
  toFetch <- getLine
  let toFetchNums = parseFetchResponse toFetch
  return $ [ i | (n,i) <- enumItems
               , tf <- toFetchNums
               , n == tf ]

-- "1 2 3 14" -> [1,2,3,14]
parseFetchResponse :: String -> [Integer]
parseFetchResponse s = map read (words s)

-- make prettier
displayItems :: [(Integer,Item)] -> IO ()
displayItems ei = let strs =  map (\(n,i) -> (show n) ++ ":  " ++ (maybe "N/A" id (getItemTitle i)) ++ "\n\t" ++ (maybe "N/A" id (getItemDescription i)) ++ "\n") ei in
   do sequence $ map putStr strs
      hFlush stdout
      return ()

-- for each item, fetch and html at itemLink
-- parse out any youtube links
-- pass them to youtube-dl
-- fetchItems :: [Items] -> IO ()
fetchItems [] = do return ()
fetchItems (i:is) = let itemLink = fromMaybe "" (getItemLink i)
                        itemTitle = fromMaybe "" (getItemTitle i) in
   do rsp <- Network.HTTP.simpleHTTP (getRequest itemLink)
      body <- getResponseBody rsp
      let links = getLinks body
      _ <- saveVODs links
      return ()

-- pull youtube links from HTML text
{- example:
<div class="series_view">
...
<embed src="http://www.youtube.com/v/wXcIqfsYveE?fs=1&amp;hl=en_US" type="application/x-shockwave-flash" allowfullscreen="true" width="640" height="385">
-}

getLinks :: String -> [String]
getLinks str = findEmbedSrcLinks seriesViewElement
 where seriesViewElement = findSeriesViewElement headEle
       headEle = head . onlyElems . parseXML $ str

findSeriesViewElement :: Element -> Element
findSeriesViewElement el = fromMaybe blank_element $ filterElement isSeriesViewDiv el
  where isSeriesViewDiv el = (qName . elName $ el) == "div"
                           && any (\attr -> (qName . attrKey $ attr) == "class"
                                   && attrVal attr == "series_view") (elAttribs el)

findEmbedSrcLinks :: Element -> [String]
findEmbedSrcLinks el = map getSrcLink embedEls where
  embedEls = filterElements (\el -> (qName . elName $ el) == "embed"
                                    && any (\attr -> (qName . attrKey $ attr) == "src") (elAttribs el) ) el
  getSrcLink :: Element -> String
  getSrcLink el' = attrVal . head $ filter (\attr -> (qName . attrKey $ attr) == "src") (elAttribs el')

-- saveVODs :: [String] -> IO ()
saveVODs ss = sequence $ map (\s -> rawSystem "youtube-dl" ["-t", "-q", s] >> return ()) ss
