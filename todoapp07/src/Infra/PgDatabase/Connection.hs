module Infra.PgDatabase.Connection where

import Control.Exception (bracket)
import Database.PostgreSQL.Simple
import Relude

withConnection :: (Connection -> IO ()) -> IO ()
withConnection action = do
  bracket
    ( connect $
        ConnectInfo
          { connectHost = "localhost",
            connectPort = 5432,
            connectDatabase = "todoapp",
            connectUser = "constacts",
            connectPassword = ""
          }
    )
    close
    action