{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module DevTools.Pipe
    ( pipe
    )
where

-- base ----------------------------------------------------------------------
import           Control.Applicative (empty)
import           Control.Concurrent (forkIO, killThread, threadDelay)
import           Control.Exception (handleJust)
import           Control.Monad (forever)
import           Data.Functor (void)
import           Data.IORef (newIORef)
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic)
import           GHC.IO.Exception (ioe_type, IOErrorType (InvalidArgument))
import           Prelude hiding (null, read)
import           System.IO (Handle, hClose, hFlush)
import           System.IO.Unsafe (unsafeInterleaveIO)


-- bytestring ----------------------------------------------------------------
import qualified Data.ByteString as B
import           Data.ByteString.Builder (Builder, hPutBuilder, word8)
import           Data.ByteString.Builder.Extra (defaultChunkSize)
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as L


-- devtools ------------------------------------------------------------------
import           DevTools.Server (Server (Server))


-- resource ------------------------------------------------------------------
import           Data.Resource (Resource, resource)


-- stm -----------------------------------------------------------------------
import           Control.Concurrent.STM.TChan
                     ( newBroadcastTChanIO, newTChanIO, readTChan, writeTChan
                     )
import           Control.Monad.STM (atomically)


-- transformers --------------------------------------------------------------
import           Control.Monad.Trans.Class (lift)


-- unix ----------------------------------------------------------------------
import           System.Posix.IO.ByteString
                     ( createPipe, dupTo
                     , openFd, closeFd, fdToHandle
                     , OpenMode (ReadWrite), defaultFileFlags
                     )
import           System.Posix.Process.ByteString
                     ( forkProcess, executeFile
                     , getProcessID, createProcessGroupFor
                     , ProcessStatus (Stopped), getProcessStatus
                     )
import           System.Posix.Signals
                     ( signalProcess, softwareTermination, killProcess
                     )
import           System.Posix.Types (Fd, ProcessID)


------------------------------------------------------------------------------
data Pipe = Pipe
    { input :: !Handle
    , output :: !Handle
    }
  deriving (Eq, Show, Generic, Typeable)


------------------------------------------------------------------------------
close :: Fd -> IO ()
close = handleJust closed pure . closeFd
  where
    closed e
        | ioe_type e == InvalidArgument = pure ()
        | otherwise = empty


------------------------------------------------------------------------------
drain :: Resource IO Fd
drain = resource open close
  where
    open = openFd "/dev/null" ReadWrite Nothing defaultFileFlags


------------------------------------------------------------------------------
pair :: Resource IO (Fd, Fd)
pair = resource createPipe $ uncurry $ \up down -> close up *> close down


------------------------------------------------------------------------------
fork :: IO () -> Resource IO ProcessID
fork run = resource (forkProcess run) wait
  where
    wait pid = do
        mstatus <- getProcessStatus False True pid
        case mstatus of
            Nothing -> kill
            Just status -> handle status
      where
        kill = do
            signalProcess softwareTermination pid
            mstatus <- getProcessStatus False True pid
            case mstatus of
                Nothing -> threadDelay 50000 *> go
                  where
                    go = do
                        signalProcess killProcess pid
                        mstatus' <- getProcessStatus False True pid
                        case mstatus' of
                            Nothing -> threadDelay 20000 *> go
                            Just status -> handle status
                Just status -> handle status
        handle (Stopped _) = kill
        handle _ = pure ()


------------------------------------------------------------------------------
chrome :: Resource IO Pipe
chrome = do
    null <- drain
    (uin, din) <- pair
    (dout, uout) <- pair
    _pid <- fork $ go null uin uout
    input <- resource (fdToHandle din) hClose
    output <- resource (fdToHandle dout) hClose
    pure $ Pipe input output
  where
    go null uin uout = do
        _pgid <- getProcessID >>= createProcessGroupFor
        void $ dupTo null 0
        void $ dupTo null 1
        void $ dupTo null 2
        void $ dupTo uin 3
        void $ dupTo uout 4
        executeFile "chromium" True args Nothing
    -- copied from puppeteer
    args =
        [ "--disable-background-networking"
        , "--enable-features=NetworkService,NetworkServiceInProcess"
        , "--disable-background-timer-throttling"
        , "--disable-backgrounding-occluded-windows"
        , "--disable-breakpad"
        , "--disable-client-side-phishing-detection"
        , "--disable-default-apps"
        , "--disable-dev-shm-usage"
        , "--disable-extensions"
        , "--disable-features=site-per-process,TranslateUI,\
            \BlinkGenPropertyTrees"
        , "--disable-hang-monitor"
        , "--disable-ipc-flooding-protection"
        , "--disable-popup-blocking"
        , "--disable-prompt-on-repost"
        , "--disable-renderer-backgrounding"
        , "--disable-sync"
        , "--force-color-profile=srgb"
        , "--metrics-recording-only"
        , "--no-first-run"
        , "--safebrowsing-disable-auto-update"
        , "--enable-automation"
        , "--password-store=basic"
        , "--use-mock-keychain"
        , "--headless"
        , "--hide-scrollbars"
        , "--mute-audio"
        , "--remote-debugging-pipe"
        ]


------------------------------------------------------------------------------
read :: Pipe -> IO [ByteString]
read (Pipe _ output) = split . L.fromChunks <$> go
  where
    split bytes
        | L.null bytes = []
        | otherwise = case L.break (== 0) bytes of
            (chunk, rest)
                | L.null rest -> [chunk]
                | otherwise -> chunk : split (L.drop 1 rest)
    go = do
        bytes <- B.hGetSome output defaultChunkSize
        case B.null bytes of
            True -> pure []
            False -> case B.last bytes of
                0 -> pure [chunk]
                  where
                    chunk = B.take (B.length bytes - 1) bytes 
                _ -> do
                    chunks <- unsafeInterleaveIO go
                    pure $ chunk : chunks
                  where
                    chunk = bytes


------------------------------------------------------------------------------
write :: Pipe -> Builder -> IO ()
write (Pipe input _) builder = do
    hPutBuilder input $ builder <> word8 0
    hFlush input


------------------------------------------------------------------------------
serve :: Pipe -> Resource IO Server
serve _pipe = do
    counter <- lift $ newIORef 0
    input <- lift newTChanIO
    output <- lift newBroadcastTChanIO
    _send <- resource (forkIO (send input)) killThread
    _recv <- resource (forkIO (recv output)) killThread
    pure $ Server counter input output
  where
    send = forever . (write _pipe =<<) . atomically . readTChan
    recv = forever . (read _pipe >>=) . foldMap . (atomically .) . writeTChan


------------------------------------------------------------------------------
pipe :: Resource IO Server
pipe = chrome >>= serve
