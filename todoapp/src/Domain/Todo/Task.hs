{-# LANGUAGE DeriveAnyClass #-}

module Domain.Todo.Task where

import Data.Text (Text, length)
import Data.Time (UTCTime)
import Data.ULID (ULID)
import GHC.Generics (Generic)
import Prelude hiding (id, length)

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
      Just $
        Task
          { id = taskId,
            content = content,
            createdAt = createdAt,
            completed = False,
            updatedAt = Nothing
          }
