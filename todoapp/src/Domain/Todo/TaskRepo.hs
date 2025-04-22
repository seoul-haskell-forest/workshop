module Domain.Todo.TaskRepo where

import Domain.Todo.Task (Task)

class TaskRepo m where
  save :: Task -> m Bool