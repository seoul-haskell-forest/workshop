{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Todo.UseCase where

import Control.Error.Util ((??))
import Control.Monad.Error.Class (MonadError (..))
import Data.Time (getCurrentTime)
import Data.ULID (getULID)
import Domain.Todo.Task (Task, mkTask)
import Domain.Todo.TaskRepo (TaskRepo)
import qualified Domain.Todo.TaskRepo as TaskRepo
import Relude hiding ((??))

createTask :: (MonadIO m, TaskRepo m) => Text -> m (Either Text Task)
createTask content = runExceptT $ do
  taskId <- liftIO getULID
  now <- liftIO getCurrentTime
  task <- mkTask taskId content now ?? "Task content must be less than 12 characters"
  result <- lift $ TaskRepo.save task
  if result
    then pure task
    else throwError "Task save failed"
