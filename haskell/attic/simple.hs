{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}

import Control.Concurrent (forkIO,threadDelay)
import Control.Concurrent.STM
import Control.Exception (finally)
import Control.Monad.Error
import Control.Applicative

import Text.Printf (printf)

data LogJob = LogMsg String | LogDone
data Result = Res String | Ready

data Task =
  Page {name :: String,
        links :: [Task]}
  | Done
  deriving (Eq,Show)
p = Page "one" [
      Page ".two" [
        Page "..four" [],
        Page "..five" []
      ],
      Page ".three" [
        Page "..six" [],
        Page "..seven" []
      ]]
main = do
    results <- newTChanIO
    -- for sending jobs to workers
    jobQueue <- newTChanIO
    logChannel <- newTChanIO

    activeLogging <- newTVarIO 1
    _ <- forkIO $
      (logService logChannel)
      `finally`
      (atomically $ modifyTVar activeLogging (subtract 1))

    let logSync s = atomically $ writeTChan logChannel (LogMsg s)

    let k = 4
    -- the number of workers currently running
    activeWorkers <- newTVarIO k

    logSync (printf "start %d threads...\n" k)
    -- start worker threads
    forM_ [1..k] $ \n -> forkIO $
      (worker logSync results jobQueue n)
      `finally`
      (atomically $ modifyTVar activeWorkers (subtract 1))

    -- add element to queue
    atomically $ writeTChan jobQueue p

    let loop = do
            (r, jobsDone) <- atomically $ (,) <$> readTChan results <*> isEmptyTChan jobQueue
            case r of
              Res m -> logSync ("result was " ++ m ++ ", loop...") >> loop
              Ready -> do
                logSync "ready"
                if jobsDone then do
                  logSync "finishing workers..."
                  atomically $ replicateM_ k (writeTChan jobQueue Done)
                  waitFor activeWorkers
                  atomically $ (writeTChan logChannel LogDone)
                  waitFor activeLogging
                else (logSync $ "more mainloop") >> loop
    loop

logService :: TChan LogJob -> IO ()
logService c = loop
  where loop = do
          x <- atomically $ readTChan c
          case x of
            LogMsg m -> print m >> loop
            LogDone -> print "done logging!" >> return ()

waitFor :: TVar Int -> IO ()
waitFor alive = atomically $ do
  count <- readTVar alive
  check (count == 0)

worker :: (String -> IO ()) ->
          TChan Result ->
          TChan Task ->
          Int ->
          IO ()
worker logSync results jobQueue i = loop
  where
    -- Consume jobs until we are told to exit.
    loop = do
        job <- atomically $ readTChan jobQueue
        logSync $ printf "[worker %d] consuming another job" i
        threadDelay $ 100*1000
        case job of
            Done  -> return ()
            (Page n ls) -> do
              logSync (printf "received page %s" n)
              if (not . null) ls
                then atomically $ mapM_ (writeTChan jobQueue) ls >> writeTChan results (Res $ "checked " ++ n)
                else atomically $ writeTChan results (Ready)
              loop




