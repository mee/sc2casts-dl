module Main where

import Codec.Compression.GZip (decompress)
import Control.Monad (liftM)
import Data.Bits.Utils (s2w8, w82s)
import Data.ByteString.Internal (ByteString)
import Data.List (isInfixOf, any)
import Data.Maybe
import Network.BufferType
import Network.HTTP
import Network.HTTP.Headers
import Network.Stream (Result)
import Network.URI
import System.Cmd
import System.IO
import Text.Atom.Feed
import Text.Feed.Constructor
import Text.Feed.Import
import Text.Feed.Query
import Text.Feed.Types
import Text.XML.Light
import qualified Data.ByteString.Char8 as B (pack, unpack)
import qualified Data.ByteString.Lazy as BL

{- Example item:
(22,RSSItem (RSSItem {rssItemTitle = Just "Zerker vs Spades (1 Game)", rssItemLink = Just "http://sc2casts.com/cast7028-Zerker-vs-Spades-1-Game-Starcraft-Ladder-Battle.net-VOD", rssItemDescription = Just "[ZvT] Starcraft Ladder - Battle.net VOD / cast by: NanMan @ YouTube", rssItemAuthor = Nothing, rssItemCategories = [], rssItemComments = Nothing, rssItemEnclosure = Nothing, rssItemGuid = Just (RSSGuid {rssGuidPermanentURL = Nothing, rssGuidAttrs = [], rssGuidValue = "7028"}), rssItemPubDate = Just "Mon, 09 Jan 2012 00:00:00 +0100", rssItemSource = Nothing, rssItemAttrs = [], rssItemOther = []}))
-}

main :: IO ()
main = do
  putStr "Fetching Feed..."
  
  let myURI = URIAuth { uriUserInfo = ""
                      , uriRegName = "sc2casts.com"
                      , uriPort = ":80" }
  let myReq = mkRequest GET URI { uriScheme = "http:"
                                , uriAuthority = Just myURI
                                , uriPath = "/rss"
                                , uriQuery = ""
                                , uriFragment = "" } :: Request_String

  result <- Network.HTTP.simpleHTTP myReq
  let response = either (\_ -> error "Connection Error") id result
  rawResponse <- getResponseBody result
  
  -- is the response gzip encoded?
  let myResultHeaders = (map hdrValue (retrieveHeaders HdrContentEncoding response))
  let body = if any (isInfixOf "gzip") myResultHeaders then
               decompressString rawResponse else
               rawResponse where
                 decompressString = w82s . BL.unpack . decompress . BL.pack . s2w8

  putStrLn "Done."
  let feed = parseFeedString body
  let items = maybe [newItem . RSSKind . Just $ "fail"] feedItems feed
  let numItems = length items    
  putStrLn $ "Found " ++ show numItems ++ " items."
  selections <- selectFromItems items
  putStr $ "Fetching: " ++ (show . length $ selections) ++ " Items..."
  _ <- fetchItems selections
  return ()

selectFromItems :: [Item] -> IO [Item]
selectFromItems items | null items = return []
                      | otherwise = do
  let enumItems = zip [1..] items
  displayItems enumItems
  putStr "Fetch: "
  hFlush stdout
  toFetch <- getLine
  let toFetchNums = parseFetchResponse toFetch
  return [ i | (n,i) <- enumItems
             , tf <- toFetchNums
             , n == tf ]

-- "1 2 3 14" -> [1,2,3,14]
parseFetchResponse :: String -> [Integer]
parseFetchResponse s = map read (words s)

-- make prettier
displayItems :: [(Integer,Item)] -> IO ()
displayItems ei = let strs =  map (\(n,i) -> show n ++ ":  " ++ fromMaybe "N/A" (getItemTitle i) ++ "\n\t" ++ fromMaybe "N/A" (getItemDescription i) ++ "\n") ei in
   do mapM putStr strs
      hFlush stdout
      return ()

fetchItems :: [Item] -> IO ()
fetchItems is = do _ <- mapM fetchItem is
                   putStrLn "Done."
                   return ()

fetchItem :: Item -> IO ()
fetchItem i = let itemLink = fromMaybe "" (getItemLink i)
                  itemTitle = fromMaybe "" (getItemTitle i) in
   do rsp <- Network.HTTP.simpleHTTP (getRequest itemLink)
      body <- getResponseBody rsp
      let links = getLinks body
      _ <- saveVODs links
      return ()

-- pull youtube links from HTML text
{- example:
<div class="series_view"> ... <embed src="http://www.youtube.com/v/wXcIqfsYveE?fs=1&amp;hl=en_US" type="application/x-shockwave-flash" allowfullscreen="true" width="640" height="385"> ... </div>
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

saveVODs :: [String] -> IO [()]
saveVODs ss = mapM (\s -> rawSystem "youtube-dl" ["-t", "-q", s] >> return ()) ss
