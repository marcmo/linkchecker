{-# LANGUAGE OverloadedStrings #-}
module HttpUtils
        ( getLinks
        , pingUrl
        , canonicalizeLink
        ) where

import DataTypes
import Control.Exception (handle, SomeException(..))
import Data.Typeable ( typeOf )
import Control.Applicative
import Data.List(stripPrefix, find)
import qualified Data.Set as S
import Data.Maybe(mapMaybe, fromMaybe)
import Network.HTTP.Client
import Network.HTTP.Types.Status
import Network.URI
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Char8 as BS
import Prelude hiding (concat, putStrLn)
import Text.Regex.Posix
import Data.Array((!))

import Text.Printf (printf)

handleAny :: (SomeException -> IO a) -> IO a -> IO a
handleAny = handle

extractLinks :: URI -> B.ByteString -> S.Set URL
extractLinks url = S.fromList .
                   mapMaybe (canonicalizeLink url . fst . (!1)) .
                   matchAllText r
  where pat = "href[ ]*=[ ]*\"([^\"]*)\"" :: B.ByteString
        r = makeRegex pat :: Regex

getLinks :: Manager -> URL -> URL -> IO (Either String (S.Set URL))
getLinks man baseUrl url =
      case parseURI (B.unpack url) of
        Nothing -> return $ Left "invalid URL"
        Just uri -> do
          ping <- pingUrl man url
          case ping of
            Left e -> return $ Left e
            Right contentType
                  | not $ "text/html" `BS.isPrefixOf` contentType ->
                      return $ Left "[200] (not an HTML doc)"
                  | not $ url `belongsTo` baseUrl ->
                      return $ Left "[200] (outside link)"
                  | otherwise -> do
                      eitherLs <- getLinksFromUrl man url uri
                      return $ case eitherLs of
                        Left s -> Left s
                        Right ls -> Right ls

getLinksFromUrl :: Manager -> URL -> URI -> IO (Either String (S.Set URL))
getLinksFromUrl man url uri = getBody man url >>= \b -> return $ extractLinks uri <$> b

pingUrl :: Manager -> URL -> IO (Either String BS.ByteString)
pingUrl man u =
    handleAny anyHandler $
        handle handler $ do
            initReq <- parseUrl $ B.unpack u
            let req = initReq { method = "HEAD" }
            res <- httpNoBody req man
            let ma = snd <$> find (\x -> fst x == "Content-Type") (responseHeaders res)
            return $ fromMaybe
              (Left "could not retrieve content type")
              (Right <$> ma)
        where handler (StatusCodeException s _ _) = return (Left $ printf "[%d] StatusCodeException: %s"
                                                      (statusCode s) (BS.unpack $ statusMessage s))
              handler e = return (Left $ "not possible to ping " ++ show e)
              anyHandler e = return (Left $ "exception happended " ++ show (typeOf e))

getBody :: Manager -> URL -> IO (Either String B.ByteString)
getBody man u = do
        req <- parseUrl $ B.unpack u
        (Right . responseBody) <$> httpLbs req man

canonicalizeLink :: URI -> URL -> Maybe URL
canonicalizeLink referer _path =
  if "#" `B.isPrefixOf` _path then Nothing
    else do
      p <- parseURIReference (B.unpack _path)
      let n = p `nonStrictRelativeTo` referer
      _auth <- uriAuthority n
      let res = uriScheme n ++ "//" ++ uriUserInfo _auth ++ uriRegName _auth ++ uriPort _auth ++ uriPath n
      let res' = if last res == '/' then take (length res - 1) res else res
      return $  B.pack res'

belongsTo :: URL -> URL -> Bool
belongsTo url _host =
  isRelativeReference (B.unpack url) || regName url == regName _host
  where regName :: URL -> Maybe String
        regName u = (removeWWW . uriRegName) <$> (parseURI (B.unpack u) >>= uriAuthority)
        removeWWW :: String -> String
        removeWWW u = fromMaybe u (stripPrefix "www." u)

