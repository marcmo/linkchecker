{-# LANGUAGE OverloadedStrings #-}

import HttpUtils
import DataTypes
import Network(withSocketsDo)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.List(delete)
import qualified Data.Set as S
import Network.URI
import Data.Maybe(fromJust)
import Control.Concurrent.Async
import Text.Printf (printf)
import Network.HTTP.Client
import Network.HTTP.Client.TLS

data Task = Page {orign :: URL, name :: URL} | Done
  deriving (Eq,Show)

baseUrl = "http://esrlabs.com"
baseUri = fromJust $ parseURI (B.unpack baseUrl)
httpSettings = tlsManagerSettings {
          managerResponseTimeout = Just (5 * 1000 * 1000),
          managerConnCount = 10 }
type QueryResult = Either LinkResult (URL, S.Set URL)

main = withSocketsDo $ do
    man <- newManager httpSettings
    loop man [("",baseUrl)] [] S.empty []
    where loop :: Manager -> [(URL, URL)] -> [Async QueryResult] -> S.Set URL -> [LinkResult] -> IO ()
          loop _ [] [] _ results = do
            print "DONE!!!"
            mapM_ (\(i,res) -> printf "%d: %s\n" i (show res)) (zip ([1..] :: [Int]) results)
          loop man jobs as seen results = do
            newAs <- mapM (async . collect man) jobs
            printf "looping with %d new jobs (%d+%d threads are live)\n" (length jobs) (length as) (length newAs)
            (a, result) <- waitAny (as ++ newAs)
            case result of
              Left m -> do
                printf "%s\n" (show m)
                loop man [] (a `delete` as ++ newAs) seen (m:results)
              Right (origin, found) -> do
                let newLinks = found `S.difference` seen
                loop man
                    (zip (repeat origin) (S.toList newLinks))
                    (a `delete` as ++ newAs)
                    (newLinks `S.union` seen)
                    (OK (origin,"[200]"):results)

          collect :: Manager -> (URL, URL) -> IO QueryResult
          collect man (referee, url) = do
            eitherLs <- getLinks man baseUrl url
            return $ case eitherLs of
              Left m -> Left m
              Right ls -> Right (referee, ls)



