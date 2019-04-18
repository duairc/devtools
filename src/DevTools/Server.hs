{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module DevTools.Server
    ( Server (Server), Session, call, listen
    , ParseException (ParseException), ProtocolException (ProtocolException)
    )
where

-- aeson ---------------------------------------------------------------------
import           Data.Aeson
                     ( FromJSON, parseJSON, ToJSON, toEncoding, toJSON
                     , genericToEncoding, defaultOptions, fromEncoding
                     , withObject, (.:), (.:?)
                     , eitherDecode
                     )
import qualified Data.Aeson as A
import qualified Data.Aeson.Encoding as A
import           Data.Aeson.Types (parseEither)


-- async ---------------------------------------------------------------------
import           Control.Concurrent.Async (Async, async)


-- base ----------------------------------------------------------------------
import           Control.Applicative ((<|>), empty)
import           Control.Exception (Exception, throwIO)
import           Data.IORef (IORef, atomicModifyIORef')
import           Data.Maybe (catMaybes)
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
type Session = Text


------------------------------------------------------------------------------
data Outgoing a = Outgoing
    { id :: !Word
    , method :: !Text
    , params :: !a
    , sessionId :: !(Maybe Session)
    }
  deriving
    ( Eq, Ord, Read, Show, Generic, Typeable
    , NFData, Hashable
    , Generic1, Foldable, Functor, Traversable
    )


------------------------------------------------------------------------------
outgoingPairs :: ToJSON a => Outgoing a -> [Pair]
outgoingPairs (Outgoing i m p s) = catMaybes
    [ "id" .=. i, "method" .=. m, "params" .=. p, "sessionId" .=? s
    ]


------------------------------------------------------------------------------
instance ToJSON a => ToJSON (Outgoing a) where
    toEncoding = encoding . outgoingPairs
    toJSON = object . outgoingPairs


------------------------------------------------------------------------------
instance FromJSON a => FromJSON (Outgoing a) where
    parseJSON = withObject "Outgoing" $ \o -> Outgoing
        <$> o .: "id" <*> o .: "method" <*> o .: "params"
        <*> o .:? "sessionId"


------------------------------------------------------------------------------
data Incoming a
    = Error !ProtocolException
    | Event
        { method :: !Text
        , params :: !a
        , sessionId :: !(Maybe Session)
        }
    | Result
        { id :: !Word
        , result :: !a
        , sessinId :: !(Maybe Session)
        }
  deriving
    ( Eq, Ord, Read, Show, Generic, Typeable
    , NFData, Hashable
    , Generic1, Foldable, Functor, Traversable
    )


------------------------------------------------------------------------------
incomingPairs :: ToJSON a => Incoming a -> [Pair]
incomingPairs (Error e) = ["error" .= e]
incomingPairs (Event m p s) = catMaybes
    [ "method" .=. m, "params" .=. p, "sessionId" .=? s
    ]
incomingPairs (Result i r s) = catMaybes
    [ "id" .=. i, "result" .=. r, "sessionId" .=? s
    ]


------------------------------------------------------------------------------
instance ToJSON a => ToJSON (Incoming a) where
    toEncoding = encoding . incomingPairs
    toJSON = object . incomingPairs


------------------------------------------------------------------------------
instance FromJSON a => FromJSON (Incoming a) where
    parseJSON v = error <|> event <|> result
      where
        error = withObject "Error" (fmap Error . (.: "error")) v
        event = withObject "Event" go v
          where
            go o = Event <$> o .: "method" <*> o .: "params"
                <*> o .:? "sessionId"
        result = withObject "Result" go v
          where
            go o = Result <$> o .: "id" <*> o .: "result"
                <*> o .:? "sessionId"


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
call :: (ToJSON a, FromJSON b)
    => Server -> Maybe Session -> Text -> a -> IO (Async b)
call server@(Server counter input _) session method params = do
    id <- atomicModifyIORef' counter $ \n -> (n + 1, n)
    atomically $ writeTChan input $ encode $ Outgoing id method params session
    incoming server $ select id
  where
    encode = fromEncoding . toEncoding
    select id (Result id' result session')
        | id == id' && session == session' = pure result
    select _ _ = empty


------------------------------------------------------------------------------
listen :: FromJSON a => Server -> Maybe Session -> Text -> IO (Async a)
listen server session method = incoming server select
  where
    select (Event method' params session')
        | method == method' && session == session' = pure params
    select _ = empty


------------------------------------------------------------------------------
data Pair = Pair !Text !A.Encoding !A.Value


------------------------------------------------------------------------------
encoding :: [Pair] -> A.Encoding
encoding = A.pairs . foldMap go
  where
    go (Pair key value _) = A.pair key value


------------------------------------------------------------------------------
object :: [Pair] -> A.Value
object = A.object . map go
  where
    go (Pair key _ value) = (key, value)


------------------------------------------------------------------------------
(.=) :: ToJSON a => Text -> a -> Pair
(.=) key = Pair key <$> toEncoding <*> toJSON


------------------------------------------------------------------------------
(.=.) :: ToJSON a => Text -> a -> Maybe Pair
(.=.) = fmap pure . (.=)
infixr 8 .=.


------------------------------------------------------------------------------
(.=?) :: ToJSON a => Text -> Maybe a -> Maybe Pair
(.=?) = fmap . (.=)
infixr 8 .=?
