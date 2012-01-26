module Main where

import Codec.Compression.GZip (decompress)
import Control.Monad (liftM)
import Data.Bits.Utils (s2w8, w82s)
import Data.Char (isDigit)
import Data.List (isInfixOf, any)
import Data.Maybe
import Network.HTTP
import Network.URI
import System.Cmd
import System.IO
import Text.Feed.Import
import Text.Feed.Query
import Text.Feed.Types
import Text.XML.Light
import qualified Data.ByteString.Lazy as BL

{- Example item:
(22,RSSItem (RSSItem {rssItemTitle = Just "Zerker vs Spades (1 Game)", rssItemLink = Just "http://sc2casts.com/cast7028-Zerker-vs-Spades-1-Game-Starcraft-Ladder-Battle.net-VOD", rssItemDescription = Just "[ZvT] Starcraft Ladder - Battle.net VOD / cast by: NanMan @ YouTube", rssItemAuthor = Nothing, rssItemCategories = [], rssItemComments = Nothing, rssItemEnclosure = Nothing, rssItemGuid = Just (RSSGuid {rssGuidPermanentURL = Nothing, rssGuidAttrs = [], rssGuidValue = "7028"}), rssItemPubDate = Just "Mon, 09 Jan 2012 00:00:00 +0100", rssItemSource = Nothing, rssItemAttrs = [], rssItemOther = []}))
-}

main :: IO ()
main = do
  body <- getFeedBody

  let items = maybe [] feedItems (parseFeedString body)
  putStrLn $ "Found " ++ (show . length $ items) ++ " items."
  selections <- selectFromItems items

  putStrLn $ "Fetching " ++ (show . length $ selections) ++
    " Item" ++ (if length selections > 1 then "s." else ".")
  hFlush stdout
  mapM_ fetchItem selections
  putStrLn "Finished."

getFeedBody :: IO String
getFeedBody = do
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
  return body

selectFromItems :: [Item] -> IO [Item]
selectFromItems items | null items = return []
                      | otherwise = do
  let enumItems = zip [1..] items
  displayItems enumItems
  putStr "Fetch: "
  hFlush stdout
  toFetch <- getLine
  let toFetchNums = parseFetchResponse (map fst enumItems) toFetch
  return [ i | (n,i) <- enumItems
             , tf <- toFetchNums
             , n == tf ]

parseFetchResponse :: [Integer] -> String -> [Integer]
parseFetchResponse l s = filter isValid $ map safeRead (words s)
  where safeRead w = if all isDigit w then read w else 0
        isValid n = and [ n `elem` l, n > 0 ]

prop_parseFetchResponse :: [Integer] -> Bool
prop_parseFetchResponse x = parseFetchResponse x (unwords $ map show x) == filter (>0) x

-- make prettier
displayItems :: [(Integer,Item)] -> IO ()
displayItems ei = let numColumns = succ . length . show . maximum $ map fst ei
                      pad n s = if length s < n
                                then (s ++ (concat $ take (n - length s) $ repeat " "))
                                else s
                      strs = map (\(n,i) -> pad (numColumns+1) (show n ++ ":") ++
                                            fromMaybe "N/A" (getItemTitle i) ++ "\n" ++
                                            (pad (numColumns+2) "") ++
                                            fromMaybe "N/A" (getItemDescription i) ++ "\n") ei in
                  do mapM_ putStr strs
                     hFlush stdout
                     return ()

fetchItem :: Item -> IO ()
fetchItem i = let itemLink = fromMaybe "" (getItemLink i)
                  itemTitle = fromMaybe "" (getItemTitle i) in
   do putStr $ "Fetching " ++ itemTitle ++ "..."
      hFlush stdout
      rsp <- Network.HTTP.simpleHTTP (getRequest itemLink)
      body <- getResponseBody rsp
      let links = getLinks body
      _ <- saveVODs links
      putStrLn "Done."

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

-- this should put them in a directory and create a playlist, so you
-- can watch them without knowing how many games there are
saveVODs :: [String] -> IO [()]
saveVODs ss = mapM (\s -> rawSystem "youtube-dl" ["-t", "-q", s] >> return ()) ss
