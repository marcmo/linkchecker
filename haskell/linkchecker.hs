{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Exception (finally, handle, SomeException)
import Control.Monad.Error
import Control.Applicative
import Data.List(stripPrefix)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe(mapMaybe, fromJust)
import Network.URI
import Network.HTTP.Client(HttpException(..))
import Text.HTML.TagSoup
import Network.Wreq
import Control.Lens
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Char8 as BS

import Text.Printf (printf)

handleAny :: (SomeException -> IO a) -> IO a -> IO a
handleAny = handle

data LogJob = LogMsg String | LogDone
data LinkResult = OK String | LinkError String | FormatError String deriving (Show,Eq)
type Result = M.Map URL [(URL,LinkResult)]
type Logging = String -> IO ()
type URL = B.ByteString
maxredirects = 4

data Task = Page {orign :: URL, name :: URL} | Done
  deriving (Eq,Show)

extractLinks :: URI -> B.ByteString -> S.Set URL
extractLinks url = S.fromList .
            mapMaybe (canonicalizeLink url) .
            filter (not . B.null) .
            map (fromAttrib "href") .
            filter (isTagOpenName "a") .
            canonicalizeTags .
            parseTags

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
belongsTo url host =
  isRelativeReference (B.unpack url) || regName url == regName host
regName :: B.ByteString -> Maybe String
regName u = do
  uri <- parseURI (B.unpack u)
  a <- uriAuthority uri
  return $ removeWWW (uriRegName a)
-- regName u = uriRegName <$> (removeWWW >>= parseURI (B.unpack u) >>= uriAuthority)
removeWWW :: String -> String
removeWWW u = case (stripPrefix "www." u) of
  Just r -> r
  Nothing -> u

pingUrl :: URI -> IO (Either String Int)
pingUrl u = handle handler $ do
          let opts = defaults & redirects .~ maxredirects
          resp <- headWith opts (show u)
          return $ Right (resp ^. responseStatus . statusCode)
      where handler (StatusCodeException s _ _) = return (Left $ "StatusCodeException: " ++ show s)
            handler e = return (Left $ "not possible to ping " ++ show u ++ show e)

getContentType :: URI -> IO (Either String (BS.ByteString))
getContentType u =
      handleAny (\_ -> return (Left $ "no content type available for " ++ show u)) $ do
          let opts = defaults & redirects .~ maxredirects
          resp <- headWith opts (show u)
          return $ Right (resp ^. responseHeader "Content-Type")

getBody :: URI -> IO (Either String B.ByteString)
getBody u = do
      let opts = defaults & redirects .~ maxredirects
      resp <- getWith opts (show u)
      return $ Right (resp ^. responseBody)

getLinksFromUrl :: URI -> IO (Either String (S.Set URL))
getLinksFromUrl uri = do
        b <- getBody uri
        case b of
          Left e -> return $ Left e
          Right content -> do
            let eitherLs = extractLinks uri content
            return $ Right eitherLs

baseUrl = "http://esrlabs.com"
-- baseUrl = "http://bit.ly/1ePILXw"
baseUri = fromJust $ parseURI (B.unpack baseUrl)

-- baseUrl = "1"
main = do
    results <- atomically newEmptyTMVar
    jobQueue <- newTQueueIO
    logChannel <- newTQueueIO

    seen <- atomically $ newTVar (S.singleton baseUrl :: S.Set URL)
    activeLogging <- atomically $ newTVar 1
    _ <- forkIO $ logService logChannel `finally`
        atomically (modifyTVar activeLogging (subtract 1))

    let logSync s = atomically $ writeTQueue logChannel (LogMsg s)

    let k = 50
    -- the number of workers currently running
    runningWorkers <- atomically $ newTVar k
    activeCount <- atomically $ newTVar 0

    logSync (printf "start %d threads...\n" k)
    -- start worker threads
    forM_ [1..k] $ \n -> forkIO $
      worker logSync results jobQueue seen n activeCount
      `finally`
      atomically (modifyTVar runningWorkers (subtract 1))

    -- add element to queue
    atomically $ writeTQueue jobQueue (Page "" baseUrl)

    let mainloop rs = do
          (r, jobsDone) <- atomically $ (,) <$> takeTMVar results <*> isEmptyTQueue jobQueue
          let rs' = r:rs
          if not jobsDone then mainloop rs'
            else do
              activeC <- readTVarIO activeCount
              if activeC /= 0 then mainloop rs'
                else do
                  logSync "=============> finishing workers..."
                  atomically $ replicateM_ k (writeTQueue jobQueue Done)
                  waitFor runningWorkers
                  atomically $ writeTQueue logChannel LogDone
                  waitFor activeLogging
                  print "++++ results ++++"
                  forM_ (zip [1..] (reverse rs')) $ \(i,r') ->
                    printf "%s: %s\n" (show i) (show r')

    mainloop []

logService :: TQueue LogJob -> IO ()
-- logService c = return ()
logService c = loop
  where loop = do
          x <- atomically $ readTQueue c
          case x of
            LogMsg m -> putStrLn m >> loop
            LogDone -> void $ putStrLn "done logging!"

waitFor :: TVar Int -> IO ()
waitFor alive = atomically $ do
  count <- readTVar alive
  check (count == 0)

addIfPossible :: TVar (S.Set URL) -> URL -> STM Bool
addIfPossible seen link = do
  seenLinks <- readTVar seen
  if link `S.member` seenLinks
    then return False
    else writeTVar seen (link `S.insert` seenLinks) >> return True


worker :: Logging ->
          TMVar LinkResult ->
          TQueue Task ->
          TVar (S.Set URL) ->
          Int ->
          TVar Int ->
          IO ()
worker logSync results jobQueue seen i activeCount = loop
    where
      -- Consume jobs until we are told to exit.
      loop :: IO ()
      loop = handleAny (\e -> do
        atomically $ putTMVar results (LinkError $ "Got an exception: " ++ show e)
            >> modifyTVar activeCount (subtract 1)
        return ()) $ do
          job <- atomically $ modifyTVar activeCount (+1) >> readTQueue jobQueue
          let report e = putTMVar results e >> modifyTVar activeCount (subtract 1)
          case job of
              Done  -> do
                logSync (printf "SHUTDOWN")
                atomically $ modifyTVar activeCount (subtract 1)
                return ()
              (Page orig url) -> do
                logSync (printf "[worker%d] working on page %s (source %s)" i (B.unpack url) (B.unpack orig))
                case parseURI (B.unpack url) of
                  Nothing -> atomically $ report (FormatError ("invalid URL:" ++ B.unpack url))
                  Just uri -> do
                    ping <- pingUrl uri
                    case ping of
                      Left e -> atomically $ report (LinkError e)
                      Right code -> do
                        r <- getContentType uri
                        case r of
                          Left e -> atomically $ report (LinkError e)
                          (Right contentType)
                            | code /= 200 -> atomically $ report (LinkError $ "HTML Error " ++ show code)
                            | "text/html" `BS.isPrefixOf` contentType && url `belongsTo` baseUrl -> do
                                  eitherLs <- getLinksFromUrl uri
                                  case eitherLs of
                                    Left m -> atomically $ report (LinkError m)
                                    Right ls -> do
                                      let cleanedLinks = mapMaybe (canonicalizeLink baseUri) (S.toList ls)
                                      atomically $ do
                                        newLinks <- filterM (addIfPossible seen) cleanedLinks
                                        report (OK $ printf "worker%d checked:%s (%d new links)" i (B.unpack url) (length newLinks))
                                        mapM_ (writeTQueue jobQueue . Page url) newLinks
                            | otherwise -> atomically $ report (OK $ "stopping at " ++ B.unpack url)
                loop


