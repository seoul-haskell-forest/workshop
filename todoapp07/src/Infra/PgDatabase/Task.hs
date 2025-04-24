module Infra.PgDatabase.Task where

import Data.ULID (ULID)
import Database.PostgreSQL.Simple (Connection, FromRow, ToRow, execute)
import Database.PostgreSQL.Simple.FromField (FromField (fromField))
import Database.PostgreSQL.Simple.ToField (ToField (toField))
import Domain.Todo.Task (Task)
import Relude

instance ToRow Task

instance ToField ULID where
  toField ulid = toField (show ulid :: Text)

instance FromRow Task

instance FromField ULID where
  fromField f mbs = fromMaybe (error "ULID parse error") . readMaybe <$> fromField f mbs

insertTask :: Connection -> Task -> IO Bool
insertTask conn task = do
  result <- execute conn "INSERT INTO tasks (id, content, completed, craeted_at, updated_at) VALUES (?, ?, ?, ?, ?)" task
  pure $ result == 1
