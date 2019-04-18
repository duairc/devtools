{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module DevTools.Method
    ( Method (Result, name)
    )
where

-- aeson ---------------------------------------------------------------------
import           Data.Aeson (ToJSON)


-- devtools ------------------------------------------------------------------
import           DevTools.Reply (FromJSON)


-- text ----------------------------------------------------------------------
import           Data.Text (Text)


------------------------------------------------------------------------------
class (ToJSON a, FromJSON (Result a)) => Method a where
    type Result a
    name :: proxy a -> Text
