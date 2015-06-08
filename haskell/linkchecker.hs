{-# LANGUAGE OverloadedStrings #-}

import HttpUtils
import DataTypes
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Exception (finally, handle, SomeException(..))
import Control.Monad.Error
import Control.Applicative
import qualified Data.Set as S
import Data.Maybe(mapMaybe, fromJust)
import Network(withSocketsDo)
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.URI
import qualified Data.ByteString.Lazy.Char8 as B

import Text.Printf (printf)

handleAny :: (SomeException -> IO a) -> IO a -> IO a
handleAny = handle

data LogJob = LogMsg String | LogDone
type Logging = String -> IO ()
maxredirects = 4

data Task = Page {orign :: URL, name :: URL} | Done
  deriving (Eq,Show)

baseUrl = "http://esrlabs.com"
baseUri = fromJust $ parseURI (B.unpack baseUrl)

main = withSocketsDo $ do
    results <- atomically newEmptyTMVar
    jobQueue <- newTQueueIO
    logChannel <- newTQueueIO

    seen <- atomically $ newTVar (S.singleton baseUrl :: S.Set URL)
    activeLogging <- atomically $ newTVar 1
    _ <- forkIO $ logService logChannel `finally`
        atomically (modifyTVar activeLogging (subtract 1))

    let logSync s = atomically $ writeTQueue logChannel (LogMsg s)

    let k = 100
    -- the number of workers currently running
    runningWorkers <- atomically $ newTVar k
    activeCount <- atomically $ newTVar 0
    man <- newManager tlsManagerSettings {
      managerResponseTimeout = Just (5 * 1000 * 1000),
      managerConnCount = k }

    logSync (printf "start %d threads...\n" k)
    -- start worker threads
    forM_ [1..k] $ \n -> forkIO $
      worker man logSync results jobQueue seen n activeCount
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

worker :: Manager ->
          Logging ->
          TMVar LinkResult ->
          TQueue Task ->
          TVar (S.Set URL) ->
          Int ->
          TVar Int ->
          IO ()
worker man logSync results jobQueue seen i activeCount = loop
    where
      report :: LinkResult -> STM ()
      report e = putTMVar results e >> modifyTVar activeCount (subtract 1)
      -- Consume jobs until we are told to exit.
      loop :: IO ()
      loop = handleAny (\e -> do
        atomically $ report (LinkError (B.empty, "[worker" ++ show i ++ "] Got an exception: " ++ show e))
        return ()) $ do
          job <- atomically $ modifyTVar activeCount (+1) >> readTQueue jobQueue
          case job of
            Done -> do
              logSync (printf "SHUTDOWN")
              atomically $ modifyTVar activeCount (subtract 1)
              return ()
            (Page orig url) -> do
              logSync (printf "---> [worker%d] working on page %s (source %s)" i (B.unpack url) (B.unpack orig))
              eitherLs <- getLinks man baseUrl url
              case eitherLs of
                Left m -> atomically $ report (LinkError (url, m))
                Right ls -> do
                  let cleanedLinks = mapMaybe (canonicalizeLink baseUri) (S.toList ls)
                  newLinks <- atomically $ filterM (addIfPossible seen) cleanedLinks
                  atomically $ do
                    report (OK (url,printf "checked [worker%d] (%d new links)" i (length newLinks)))
                    mapM_ (writeTQueue jobQueue . Page url) newLinks
              loop

