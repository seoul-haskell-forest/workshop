module Domain.Todo.Task where

import Data.Aeson (ToJSON (..), Value (String))
import Data.Text (length)
import Data.Time (UTCTime)
import Data.ULID (ULID)
import Relude hiding (id, length)

data Task = Task
  { id :: ULID,
    content :: Text,
    completed :: Bool,
    createdAt :: UTCTime,
    updatedAt :: Maybe UTCTime
  }
  deriving (Show, Generic)

mkTask :: ULID -> Text -> UTCTime -> Maybe Task
mkTask taskId content createdAt =
  if length content > 12
    then
      Nothing
    else
      Just
        $ Task
          { id = taskId,
            content = content,
            createdAt = createdAt,
            completed = False,
            updatedAt = Nothing
          }

instance ToJSON Task

instance ToJSON ULID where
  toJSON ulid = String (show ulid)