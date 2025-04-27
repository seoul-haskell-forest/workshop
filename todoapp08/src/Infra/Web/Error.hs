module Infra.Web.Error where

import Data.Aeson
import Relude
import Web.Scotty.Trans

data Error = Error
  { error :: Text
  }
  deriving (Show, Generic)

instance ToJSON Error