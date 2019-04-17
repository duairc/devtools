{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module DevTools.Server
    ( Server (Server), call, listen
    , ParseException (ParseException), ProtocolException (ProtocolException)
    )
where

-- aeson ---------------------------------------------------------------------
import           Data.Aeson
                     ( FromJSON, parseJSON, ToJSON, toEncoding, toJSON
                     , genericToEncoding, defaultOptions, fromEncoding
                     , object, pairs, (.=)
                     , withObject, (.:)
                     , eitherDecode
                     )
import           Data.Aeson.Types (parseEither)


-- async ---------------------------------------------------------------------
import           Control.Concurrent.Async (Async, async)


-- base ----------------------------------------------------------------------
import           Control.Applicative ((<|>), empty)
import           Control.Exception (Exception, throwIO)
import           Data.IORef (IORef, atomicModifyIORef')
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic, Generic1)
import           Prelude hiding (error, id)


-- bytestring ----------------------------------------------------------------
import           Data.ByteString.Builder (Builder)
import           Data.ByteString.Lazy (ByteString)


-- deepseq -------------------------------------------------------------------
import           Control.DeepSeq (NFData)


-- hashable ------------------------------------------------------------------
import           Data.Hashable (Hashable)


-- stm -----------------------------------------------------------------------
import           Control.Concurrent.STM.TChan
                     ( TChan, dupTChan, readTChan, writeTChan
                     )
import           Control.Monad.STM (atomically)


-- text ----------------------------------------------------------------------
import           Data.Text (Text)


------------------------------------------------------------------------------
data Server = Server
    { counter :: !(IORef Word)
    , input :: !(TChan Builder)
    , output :: !(TChan ByteString)
    }
  deriving (Eq, Generic, Typeable)


------------------------------------------------------------------------------
data Outgoing a = Outgoing
    { id :: !Word
    , method :: !Text
    , params :: !a
    }
  deriving
    ( Eq, Ord, Read, Show, Generic, Typeable
    , NFData, Hashable, FromJSON
    , Generic1, Foldable, Functor, Traversable
    )


------------------------------------------------------------------------------
instance ToJSON a => ToJSON (Outgoing a) where
    toEncoding = genericToEncoding defaultOptions


------------------------------------------------------------------------------
data Incoming a
    = Error !ProtocolException
    | Event
        { method :: !Text
        , params :: !a
        }
    | Result
        { id :: !Word
        , result :: !a
        }
  deriving
    ( Eq, Ord, Read, Show, Generic, Typeable
    , NFData, Hashable
    , Generic1, Foldable, Functor, Traversable
    )


------------------------------------------------------------------------------
instance ToJSON a => ToJSON (Incoming a) where
    toEncoding (Error e) = pairs $ "error" .= e
    toEncoding (Event m p) = pairs $ "method" .= m <> "params" .= p
    toEncoding (Result i r) = pairs $ "id" .= i <> "result" .= r
    toJSON (Error e) = object ["error" .= e]
    toJSON (Event m p) = object ["method" .= m, "params" .= p]
    toJSON (Result i r) = object ["id" .= i, "result" .= r]


------------------------------------------------------------------------------
instance FromJSON a => FromJSON (Incoming a) where
    parseJSON v = error <|> event <|> result
      where
        error = withObject "Error" (fmap Error . (.: "error")) v
        event = withObject "Event" go v
          where
            go o = Event <$> o .: "method" <*> o .: "params"
        result = withObject "Result" go v
          where
            go o = Result <$> o .: "id" <*> o .: "result"


------------------------------------------------------------------------------
incoming :: FromJSON a
    => Server -> (forall b. Incoming b -> Maybe b) -> IO (Async a)
incoming (Server _ _ output) select = atomically (dupTChan output) >>= go
  where
    go broadcast = async loop
      where
        loop = do
            bytes <- atomically $ readTChan broadcast
            input <- either throw pure $ eitherDecode bytes
            case input of
                Error e -> throwIO e
                _ -> pure ()
            case select input of
                Nothing -> loop
                Just value -> either throw pure $ parseEither parseJSON value
    throw = throwIO . ParseException


------------------------------------------------------------------------------
newtype ParseException = ParseException String
  deriving
    ( Eq, Ord, Read, Show, Generic, Typeable, Exception
    , NFData, Hashable, FromJSON
    )


------------------------------------------------------------------------------
instance ToJSON ParseException where
    toEncoding = genericToEncoding defaultOptions


------------------------------------------------------------------------------
data ProtocolException = ProtocolException
    { code :: !Int
    , message :: !Text
    }
  deriving
    ( Eq, Ord, Read, Show, Generic, Typeable, Exception
    , NFData, Hashable, FromJSON
    )


------------------------------------------------------------------------------
instance ToJSON ProtocolException where
    toEncoding = genericToEncoding defaultOptions


------------------------------------------------------------------------------
call :: (ToJSON a, FromJSON b) => Server -> Text -> a -> IO (Async b)
call server@(Server counter input _) method params = do
    id <- atomicModifyIORef' counter $ \n -> (n + 1, n)
    atomically $ writeTChan input $ encode $ Outgoing id method params
    incoming server $ select id
  where
    encode = fromEncoding . toEncoding
    select id (Result id' result) | id == id' = pure result
    select _ _ = empty


------------------------------------------------------------------------------
listen :: FromJSON a => Server -> Text -> IO (Async a)
listen server method = incoming server select
  where
    select (Event method' params) | method == method' = pure params
    select _ = empty
