{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Exception (finally, handle, SomeException(..))
import Data.Typeable ( typeOf )
import Control.Monad.Error
import Control.Applicative
import Data.List(stripPrefix)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe(mapMaybe, fromJust, fromMaybe)
import Network.URI
import Network.HTTP.Client(HttpException(..))
import qualified Network.HTTP.Types.Status as Status
import Text.HTML.TagSoup
import Network.Wreq
import qualified Network.Wreq.Session as Sess
import Control.Lens
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Char8 as BS

import Text.Printf (printf)

handleAny :: (SomeException -> IO a) -> IO a -> IO a
handleAny = handle

data LogJob = LogMsg String | LogDone
data LinkResult = OK (URL,String) | LinkError (URL,String) | FormatError (URL,String) deriving (Show,Eq)
type Result = M.Map URL [(URL,LinkResult)]
type Logging = String -> IO ()
type URL = B.ByteString
maxredirects = 4

data Task = Page {orign :: URL, name :: URL} | Done
  deriving (Eq,Show)


baseUrl = "http://esrlabs.com"
baseUri = fromJust $ parseURI (B.unpack baseUrl)

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
                  mapM_ reportResult (zip [1..] (reverse rs'))

    mainloop []

reportResult :: (Int, LinkResult) -> IO ()
reportResult (i, OK (u,s)) = printf "%d: %s [200] (%s)\n" i (B.unpack u) s
reportResult (i, LinkError (u,s)) = printf "%d: %s %s\n" i (B.unpack u) s
reportResult (i, FormatError (u,s)) = printf "%d: %s FormatError (%s)\n" i (B.unpack u) s

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
      report :: LinkResult -> STM ()
      report e = putTMVar results e >> modifyTVar activeCount (subtract 1)
      -- Consume jobs until we are told to exit.
      loop :: IO ()
      loop = handleAny (\e -> do
        atomically $ report (LinkError $ (B.empty, "[worker" ++ show i ++ "] Got an exception: " ++ show e))
        return ()) $ do
          job <- atomically $ modifyTVar activeCount (+1) >> readTQueue jobQueue
          case job of
              Done -> do
                logSync (printf "SHUTDOWN")
                atomically $ modifyTVar activeCount (subtract 1)
                return ()
              (Page orig url) -> do
                logSync (printf "---> [worker%d] working on page %s (source %s)" i (B.unpack url) (B.unpack orig))
                eitherLs <- getLinks url
                case eitherLs of
                  Left m -> atomically $ report m
                  Right ls -> do
                    let cleanedLinks = mapMaybe (canonicalizeLink baseUri) (S.toList ls)
                    newLinks <- atomically $ filterM (addIfPossible seen) cleanedLinks
                    atomically $ do
                      report (OK $ (url,printf "checked [worker%d] (%d new links)" i (length newLinks)))
                      mapM_ (writeTQueue jobQueue . Page url) newLinks
                loop

getLinks :: URL -> IO (Either LinkResult (S.Set URL))
getLinks url =
      case parseURI (B.unpack url) of
        Nothing -> return $ Left (FormatError (url,"invalid URL"))
        Just uri -> Sess.withSession $ \sess -> do
          ping <- pingUrl sess uri
          case ping of
            Left e -> return $ Left (LinkError (url,e))
            Right contentType
                  | "text/html" `BS.isPrefixOf` contentType && url `belongsTo` baseUrl -> do
                    eitherLs <- getLinksFromUrl sess uri
                    case eitherLs of
                      Left s -> return $ Left (LinkError (url,s))
                      Right ls -> return $ Right ls
                  | otherwise -> return $ Left (OK $ (url,"STOPPING"))

pingUrl :: Sess.Session -> URI -> IO (Either String (BS.ByteString))
pingUrl sess u =
    handleAny anyHandler $ do
        handle handler $ do
            let opts = defaults & redirects .~ maxredirects
            resp <- Sess.headWith opts sess (show u)
            return $ Right (resp ^. responseHeader "Content-Type")
        where handler (StatusCodeException s _ _) = return (Left $ printf "[%d] StatusCodeException: %s"
                                                      (Status.statusCode s) (BS.unpack $ Status.statusMessage s))
              handler e = return (Left $ "not possible to ping " ++ show e)
              anyHandler e = return (Left $ "exception happended " ++ show (typeOf e))

getBody :: Sess.Session -> URI -> IO (Either String B.ByteString)
getBody sess u = do
      let opts = defaults & redirects .~ maxredirects
      resp <- Sess.getWith opts sess (show u)
      return $ Right (resp ^. responseBody)

getLinksFromUrl :: Sess.Session -> URI -> IO (Either String (S.Set URL))
getLinksFromUrl sess uri = getBody sess uri >>= \b -> return $ extractLinks uri <$> b

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
  where regName u = (removeWWW . uriRegName) <$> (parseURI (B.unpack u) >>= uriAuthority)
        removeWWW u = fromMaybe u (stripPrefix "www." u)

