{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module DevTools.Client
    ( Client, run, session, sync
    , Method, calling, call
    , Event, listening, listen
    )
where

-- base ----------------------------------------------------------------------
import           Control.Applicative (Alternative, empty)
import           Control.Monad (MonadPlus, (>=>))
import           Control.Monad.Fail (MonadFail)
import           Control.Monad.Fix (MonadFix)
import qualified Control.Monad.IO.Class as B (MonadIO)


-- async ---------------------------------------------------------------------
import           Control.Concurrent.Async (Async, wait)


-- devtools ------------------------------------------------------------------
import           DevTools.Event (Event)
import qualified DevTools.Event as E
import           DevTools.Method (Method)
import qualified DevTools.Method as M
import           DevTools.Reply (reply)
import           DevTools.Server (Server, Session)
import qualified DevTools.Server as D


-- layers --------------------------------------------------------------------
import           Control.Monad.Lift
import           Control.Monad.Lift.IO (MonadIO, liftIO)
import           Monad.Reader (MonadReader, ask)
import           Monad.State (MonadState, get, put)
import           Monad.Try (MonadTry)


-- resource ------------------------------------------------------------------
import           Data.Resource (Resource, resource)


-- transformers --------------------------------------------------------------
import           Control.Monad.Trans.Reader (ReaderT, runReaderT)
import           Control.Monad.Trans.State.Strict (StateT, evalStateT)


------------------------------------------------------------------------------
newtype ClientT m a = ClientT (ReaderT Server (StateT (Maybe Session) m) a)
  deriving
    ( Functor, Applicative, Alternative, Monad, MonadPlus, B.MonadIO
    , MonadFail, MonadFix, MonadReader Server, MonadState (Maybe Session)
    )


------------------------------------------------------------------------------
instance Iso1 (ClientT m) where
    type Codomain1 (ClientT m) = ReaderT Server (StateT (Maybe Session) m)
    to1 (ClientT m) = m
    from1 = ClientT


------------------------------------------------------------------------------
instance MonadTrans ClientT where
    lift = defaultLift2


------------------------------------------------------------------------------
instance MInvariant ClientT where
    hoistiso = defaultHoistiso2


------------------------------------------------------------------------------
instance MFunctor ClientT where
    hoist = defaultHoist2


------------------------------------------------------------------------------
instance MonadTransControl ClientT where
    suspend = defaultSuspend2
    resume = defaultResume2
    capture = defaultCapture2
    extract = defaultExtract2


------------------------------------------------------------------------------
type instance LayerResult ClientT =
    DefaultLayerResult2 (ReaderT Server) (StateT (Maybe Session))
type instance LayerState ClientT =
    DefaultLayerState2 (ReaderT Server) (StateT (Maybe Session))


------------------------------------------------------------------------------
type Client = ClientT IO


------------------------------------------------------------------------------
run :: Monad m => Server -> ClientT m a -> m a
run server (ClientT client) = client `runReaderT` server `evalStateT` empty


------------------------------------------------------------------------------
session :: MonadTry m => Session -> Resource (ClientT m) Session
session new = resource open close *> pure new
  where
    open = get <* put (pure new)
    close = put


------------------------------------------------------------------------------
sync :: MonadIO m => Async a -> ClientT m a
sync = lift . liftIO . wait


------------------------------------------------------------------------------
calling :: (Method a, MonadIO m) => a -> ClientT m (Async (M.Result a))
calling a = do
    server <- ask
    cookie <- get
    lift $ liftIO $ fmap reply <$> D.call server cookie name a
  where
    name = M.name [a]


------------------------------------------------------------------------------
call :: (Method a, MonadIO m) => a -> ClientT m (M.Result a)
call = calling >=> sync


------------------------------------------------------------------------------
listening :: (Event a, MonadIO m) => proxy a -> ClientT m (Async (E.Result a))
listening a = do
    server <- ask
    cookie <- get
    lift $ liftIO $ fmap reply <$> D.listen server cookie name
  where
    name = E.name a


------------------------------------------------------------------------------
listen :: (Event a, MonadIO m) => proxy a -> ClientT m (E.Result a)
listen = listening >=> sync
