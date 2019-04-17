{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module DevTools.Method
    ( Method (Result, name), call
    )
where

-- aeson ---------------------------------------------------------------------
import           Data.Aeson (ToJSON, withArray, withObject)
import qualified Data.Aeson as A
import           Data.Aeson.Types (Parser, Value (Null))


-- async ---------------------------------------------------------------------
import           Control.Concurrent.Async (Async)


-- base ----------------------------------------------------------------------
import           Control.Applicative ((<|>))


-- devtools ------------------------------------------------------------------
import           DevTools.Client (Client)
import qualified DevTools.Server as D


-- text ----------------------------------------------------------------------
import           Data.Text (Text)


-- transformers --------------------------------------------------------------
import           Control.Monad.Trans.Reader (ReaderT (ReaderT))


------------------------------------------------------------------------------
newtype Reply a = Reply {reply :: a}


------------------------------------------------------------------------------
instance FromJSON a => A.FromJSON (Reply a) where
    parseJSON = fmap Reply . parseJSON


------------------------------------------------------------------------------
class FromJSON a where
    parseJSON :: Value -> Parser a


------------------------------------------------------------------------------
instance A.FromJSON a => FromJSON a where
    parseJSON = A.parseJSON


------------------------------------------------------------------------------
instance {-# OVERLAPPING #-} FromJSON () where
    parseJSON Null = pure ()
    parseJSON v = withArray "()" go v <|> withObject "()" go v
      where
        go _ = pure ()


------------------------------------------------------------------------------
class (ToJSON a, FromJSON (Result a)) => Method a where
    type Result a
    name :: proxy a -> Text


------------------------------------------------------------------------------
call :: Method a => a -> Client (Async (Result a))
call a = ReaderT $ \server -> fmap reply <$> D.call server (name [a]) a
