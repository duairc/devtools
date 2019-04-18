{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module DevTools.Event
    ( Event (Result, name)
    )
where

-- devtools ------------------------------------------------------------------
import           DevTools.Reply (FromJSON)


-- text ----------------------------------------------------------------------
import           Data.Text (Text)


------------------------------------------------------------------------------
class FromJSON (Result a) => Event a where
    type Result a
    type Result a = a
    name :: proxy a -> Text
