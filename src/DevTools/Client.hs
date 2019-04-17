module DevTools.Client
    ( Client, run, sync
    )
where

-- async ---------------------------------------------------------------------
import           Control.Concurrent.Async (Async, wait)


-- devtools ------------------------------------------------------------------
import           DevTools.Server (Server)


-- transformers --------------------------------------------------------------
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Reader (ReaderT, runReaderT)


------------------------------------------------------------------------------
type Client = ReaderT Server IO


------------------------------------------------------------------------------
run :: Server -> Client a -> IO a
run = flip runReaderT


------------------------------------------------------------------------------
sync :: Async a -> Client a
sync = lift . wait
