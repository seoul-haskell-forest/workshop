module Domain.Todo.TaskRepo where

import Domain.Todo.Task (Task)
import Relude

class TaskRepo m where
  save :: Task -> m Bool