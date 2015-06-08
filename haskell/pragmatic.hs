{-# LANGUAGE OverloadedStrings #-}

import HttpUtils
import DataTypes
import Network(withSocketsDo)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.List(delete)
import qualified Data.Set as S
import qualified Data.Map as M
import Network.URI
import Data.Maybe(fromJust)
import Control.Concurrent.Async
import Control.Monad(forM_)
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
type QueryResult = Either (URL, String) (URL, S.Set URL)

report :: Result -> IO ()
report rs = forM_ (zip ([1..] :: [Int]) (M.keys rs)) $ \(nr, k) -> do
    let (msg, urls) = rs M.! k
    printf "%d: %s ====> %s\n" nr (show k) msg
    forM_ (S.toList urls) (printf "\t| %s\n" . show)

main = withSocketsDo $ do
    man <- newManager httpSettings
    loop man [("",baseUrl)] [] S.empty M.empty
    where loop :: Manager -> [(URL, URL)] -> [Async QueryResult] -> S.Set URL -> Result -> IO ()
          loop _ [] [] _ results = print "DONE!!!" >> report results
          loop man jobs as seen results = do
            newAs <- mapM (async . collect man . snd) jobs
            printf "%d new jobs (%d+%d threads)\n" (length jobs) (length as) (length newAs)
            (a, result) <- waitAny (as ++ newAs)
            case result of
              Left (origin, m) -> do
                loop man [] (a `delete` as ++ newAs) seen (M.insert origin (m, S.empty) results)
              Right (origin, found) -> do
                let newLinks = found `S.difference` seen
                loop man
                    (zip (repeat origin) (S.toList newLinks))
                    (a `delete` as ++ newAs)
                    (newLinks `S.union` seen)
                    (M.insert origin ("[200]", found) results)

          collect :: Manager -> URL -> IO QueryResult
          collect man url = do
            eitherLs <- getLinks man baseUrl url
            return $ case eitherLs of
              Left m -> Left (url, m)
              Right ls -> Right (url, ls)



