{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent           (forkFinally)
import           Control.Concurrent.STM.TChan
import           Control.Concurrent.STM.TVar
import           Control.Monad.Except
import           Control.Monad.STM            (check)
import qualified Data.ByteString.Char8        as BS
import           Data.List                    (intercalate)
import           GHC.Conc.Sync
import           Network.HTTP
import           System.Console.GetOpt
import           System.Environment           (getArgs, getEnv)
import           System.Exit
import           System.FilePath              (combine)
import           Text.Printf                  (printf)
import           Text.Regex.PCRE.Heavy
import           Text.Regex.PCRE.Light        (caseless, compile)

data Job = DownloadImage String String String
         | DownloadPage Int
         | JobDone
         deriving (Show)

type WorkPipeline = ExceptT String IO
type Worker a = Job -> ExceptT String IO a

writeAllTChan :: TChan a -> [a] -> STM ()
writeAllTChan queue = mapM_ (writeTChan queue)

writeAllTChanM :: (MonadIO m) => TChan a -> [a] -> m ()
writeAllTChanM queue = liftIO . atomically . writeAllTChan queue

modifyTVarAndReturn :: TVar a -> (a -> a) -> STM a
modifyTVarAndReturn v f = do
  val <- readTVar v
  writeTVar v (f val)
  return val

incrementTVar :: TVar Int -> STM Int
incrementTVar = flip modifyTVarAndReturn (+1)

getAlbumLink :: Int -> Int -> String
getAlbumLink = printf "http://joemonster.org/album/%d/strona/%d"

getImageLink :: String -> String -> String
getImageLink = printf "http://vader.joemonster.org/upload/%s/%s.jpg"

getFileName :: String -> String -> String -> String
getFileName b n e = combine b (n ++ "." ++ e)

downloadPageContent :: Int -> String -> WorkPipeline BS.ByteString
downloadPageContent 0 _   = throwError "max redirect count reached"
downloadPageContent n url = do
  resp <- liftIO $ simpleHTTP (getRequest url)
  (a,b,c) <- liftIO $ getResponseCode resp
  case a of
    3 -> redirect resp
    2 -> liftIO $ BS.pack `fmap` getResponseBody resp
    _ -> throwError $ "cannot download page: " ++ show (a * 100 + b * 10 + c)
  where
    redirect (Right resp) = case findHeader HdrLocation resp of
      Nothing   -> throwError "redirected without location"
      Just url' -> downloadPageContent (n - 1) url'

extractMaxPage :: BS.ByteString -> WorkPipeline Int
extractMaxPage content =
  case map (head . snd) $ scan regex content of
    []      -> throwError "cannot extract page identifiers"
    matched -> return $ maximum (map (read.BS.unpack) matched)
  where regex = compile "/album/\\d*/strona/(\\d*)" [caseless]

extractImageIds :: BS.ByteString -> WorkPipeline [(String, String, String)]
extractImageIds content = return (map conv $ scan regex content)
  where regex = compile "http://vader.joemonster.org/upload/(.+)/s_(.+)\\.([a-z]+)" [caseless]
        conv (_, a:b:c:_) = (BS.unpack a, BS.unpack b, BS.unpack c)

filterImageIds :: [a] -> WorkPipeline [a]
filterImageIds = return . drop 6 . reverse

listAlbumPages :: Int -> TChan Job -> WorkPipeline ()
listAlbumPages album queue =
  downloadPageContent 5 link >>= extractMaxPage >>= \m ->
    mapM (return.DownloadPage) [1..m] >>= writeAllTChanM queue
  where link = getAlbumLink album 1

listImages :: Int -> TChan Job -> Worker ()
listImages album queue (DownloadPage page) =
  downloadPageContent 5 link >>= extractImageIds >>= filterImageIds >>=
    mapM (return . pairToJob) >>= writeAllTChanM queue
  where link = getAlbumLink album page
        pairToJob (a, b, c) = DownloadImage a b c

saveImage :: FilePath -> TVar Int -> Worker ()
saveImage basePath nameQueue (DownloadImage pre img ext) = do
  let link = getImageLink pre img
  cnt <- downloadPageContent 5 link
  currName <- liftIO . atomically $ incrementTVar nameQueue
  let finalName = getFileName basePath (show currName) ext
  liftIO $ BS.writeFile finalName cnt
  return ()

consumeTillDone :: TChan Job -> Worker () -> IO ()
consumeTillDone queue worker = do
  job <- atomically $ readTChan queue
  case job of
    JobDone -> atomically $ unGetTChan queue JobDone
    x -> do
      result <- runExceptT $ worker x
      either printError return result
      consumeTillDone queue worker
      where printError err = putStrLn (message err)
            message err = "Cannot run action '" ++ show x ++ "': " ++ err

forkTimes :: Int -> TVar Int -> IO () -> IO ()
forkTimes times alive worker = mapM_ forkSingle [1..times]
  where
    forkSingle _ = forkFinally worker decAlive
    decAlive   _ = atomically $ modifyTVar alive (subtract 1)

consumeQueue :: Int -> TChan Job -> Worker () -> IO ()
consumeQueue threads queue worker = do
  atomically $ writeTChan queue JobDone
  alive <- newTVarIO threads
  forkTimes threads alive (consumeTillDone queue worker)
  atomically $ do
    i <- readTVar alive
    check (i == 0)

mainProc :: Int -> Int -> FilePath -> IO ()
mainProc album maxThreads basePath = do
  pageQueue <- newTChanIO
  imageQueue <- newTChanIO
  nameQueue <- newTVarIO 1

  pageResult <- runExceptT $ listAlbumPages album pageQueue
  either (putStrLn.("Failed!" ++)) return pageResult
  putStrLn "Pages listed!"

  consumeQueue maxThreads pageQueue (listImages album imageQueue)
  putStrLn "Images listed!"

  consumeQueue maxThreads imageQueue (saveImage basePath nameQueue)
  cnt <- atomically $ readTVar nameQueue
  putStrLn (show (cnt - 1) ++ " images saved!")

data Options = Options {
    optHelp        :: Bool
  , optConnections :: Int
  , optAlbumId     :: Maybe Int
  , optSavePath    :: FilePath
}

setHelp :: Options -> Options
setHelp opt = opt { optHelp = True }

setConnections, setAlbumId, setSavePath :: String -> Options -> Options
setConnections s opt = opt { optConnections = read s }
setAlbumId s opt = opt { optAlbumId = Just $ read s }
setSavePath s opt = opt { optSavePath = s }

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['h'] ["help"]  (NoArg  setHelp)              "Show this help message"
  , Option ['n'] ["conn"]  (ReqArg setConnections "N")   "Number of concurrent connections (default 5)"
  , Option ['a'] ["album"] (ReqArg setAlbumId     "N")   "Album identifier"
  , Option ['d'] ["dest"]  (ReqArg setSavePath    "DIR") "Destination directory (default: $HOME)"
  ]

parseOptions :: [String] -> Options -> Either String Options
parseOptions args def =
  case getOpt Permute options args of
    (opts, _, []) -> Right $ foldl (flip id) def opts
    (_, _, errs)  -> Left (intercalate "\n" errs)

getDefOpts :: IO Options
getDefOpts = do
  home <- getEnv "HOME"
  return $ Options False 5 Nothing home

main :: IO ()
main = do
  args <- getArgs
  def <- getDefOpts
  let result = parseOptions args def
  opts <- either (`printAndDie` 1) return result
  when (optHelp opts) printUsageAndDie
  case optAlbumId opts of
    Nothing -> printAndDie "Album id not specified" 2
    Just i  -> mainProc i (optConnections opts) (optSavePath opts)

  where
    helpHeader = "Usage: album-downloader [-h] [-n N] -a AlbumId"
    printAndDie msg code = putStrLn msg >> exitWith (ExitFailure code)
    printUsageAndDie = putStrLn (usageInfo helpHeader options) >> exitSuccess
