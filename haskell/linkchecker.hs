{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Exception (catch, finally, SomeException)
import Control.Monad.Error
import Control.Monad.State
import Control.Applicative
import qualified Data.Set as S
import Data.Maybe(catMaybes)
import Network.URI
import Text.HTML.TagSoup
import Network.Wreq
import Control.Lens
import qualified Data.ByteString.Lazy.Char8 as B

import System.IO (hFlush, stdout)
import Text.Printf (printf)

extractLinks :: B.ByteString -> B.ByteString -> S.Set B.ByteString
extractLinks url = S.fromList .
            catMaybes .
            map (canonicalizeLink url) .
            filter (not . B.null) .
            map (fromAttrib "href") .
            filter (isTagOpenName "a") .
            canonicalizeTags .
            parseTags

canonicalizeLink :: B.ByteString -> B.ByteString -> Maybe B.ByteString
canonicalizeLink referer _path = do
  r <- parseURI (B.unpack referer)
  p <- parseURIReference (B.unpack _path)
  let n = p `nonStrictRelativeTo` r
  return (B.takeWhile (/= '#') (B.pack $ uriToString id n ""))

type URL = B.ByteString

data Task = Check URL | Done

main = do
    let url = "http://esrlabs.com"
    case parseURI (B.unpack url) of
        Just uri -> do
            (Right code) <- getStatus uri `catch` \e -> (return . Left . show) (e :: SomeException)
            if code == 200 then
              do (Right page) <- getPage uri
                 print $ extractLinks url page
            else print "not code 200"
            print code
        Nothing -> print "nothing..."

main2 = do
    -- count of broken links
    badCount <- newTVarIO (0 :: Int)

    -- for reporting broken links
    badLinks <- newTChanIO

    -- for sending jobs to workers
    jobs <- newTChanIO

    let k = 2
    -- the number of workers currently running
    workers <- newTVarIO k

    -- one thread reports bad links to stdout
    _ <- forkIO $ writeBadLinks badLinks

    -- start worker threads
    forkTimes k workers (worker badLinks jobs badCount)

    -- read links from files, and enqueue them as jobs
    stats <- execJob (checkURLs "http://httpbin.org")
                     (JobState S.empty 0 jobs)

    -- enqueue "please finish" messages
    atomically $ replicateM_ k (writeTChan jobs Done)

    waitFor workers

    broken <- atomically $ readTVar badCount
    printf "Found %d broken links. Checked %d links (%d unique).\n"
              broken
              (linksFound stats)
              (S.size (linksSeen stats))

forkTimes :: Int -> TVar Int -> IO () -> IO ()
forkTimes k alive act =
  replicateM_ k . forkIO $
    act
    `finally`
    (atomically $ modifyTVar alive (subtract 1))

writeBadLinks :: TChan String -> IO ()
writeBadLinks c =
  forever $
    atomically (readTChan c) >>= putStrLn >> hFlush stdout

waitFor :: TVar Int -> IO ()
waitFor alive = atomically $ do
  count <- readTVar alive
  check (count == 0)

getStatus :: URI -> IO (Either String Int)
getStatus u = do
      print $ "getStatus of " ++ show u
      let opts = defaults & redirects .~ 4
      let url = show u
      resp <- headWith opts url
      let respCode = resp ^. responseStatus . statusCode
      return $ Right respCode

getPage :: URI -> IO (Either String B.ByteString)
getPage u = do
      print $ "getPage of " ++ show u
      let opts = defaults & redirects .~ 4
      let url = show u
      resp <- getWith opts url
      let body = resp ^. responseBody
      return $ Right body

worker :: TChan String -> TChan Task -> TVar Int -> IO ()
worker badLinks jobQueue badCount = loop
  where
    -- Consume jobs until we are told to exit.
    loop = do
        job <- atomically $ readTChan jobQueue
        print "[worker] consuming another job"
        case job of
            Done  -> return ()
            Check x -> checkOne (B.unpack x) >> loop

    -- Check a single link.
    checkOne url = case parseURI url of
        Just uri -> do
            code <- getStatus uri `catch` \e -> (return . Left . show) (e :: SomeException)
            case code of
                Right 200 -> return ()
                Right n   -> report (show n)
                Left err  -> report err
        _ -> report "invalid URL"

        where report s = atomically $ do
                           modifyTVar badCount (+1)
                           writeTChan badLinks (url ++ " " ++ s)

data JobState = JobState { linksSeen :: S.Set URL,
                           linksFound :: Int,
                           linkQueue :: TChan Task }

newtype Job a = Job { runJob :: StateT JobState IO a }
    deriving (Monad, MonadState JobState, MonadIO)

instance Functor Job where
    fmap = liftM
instance Applicative Job where
    pure  = return
    (<*>) = ap

execJob :: Job a -> JobState -> IO JobState
execJob = execStateT . runJob

checkURLs :: B.ByteString -> Job ()
checkURLs url = do
    let urls = [url]
    filterM seenURI urls >>= sendJobs
    updateStats (length urls)

updateStats :: Int -> Job ()
updateStats a = modify $ \s ->
    s { linksFound = linksFound s + a }

-- | Add a link to the set we have seen.
insertURI :: URL -> Job ()
insertURI c = modify $ \s ->
    s { linksSeen = S.insert c (linksSeen s) }

-- | If we have seen a link, return False.  Otherwise, record that we
-- have seen it, and return True.
seenURI :: URL -> Job Bool
seenURI url = do
    seen <- (not . S.member url) `liftM` gets linksSeen
    insertURI url
    return seen

sendJobs :: [URL] -> Job ()
sendJobs js = do
    c <- gets linkQueue
    liftIO . atomically $ mapM_ (writeTChan c . Check) js


