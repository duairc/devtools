{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module DevTools.Reply
    ( FromJSON, reply
    )
where

-- aeson ---------------------------------------------------------------------
import           Data.Aeson (withArray, withObject)
import qualified Data.Aeson as A
import           Data.Aeson.Types (Parser, Value (Null))


-- base ----------------------------------------------------------------------
import           Control.Applicative ((<|>))


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
