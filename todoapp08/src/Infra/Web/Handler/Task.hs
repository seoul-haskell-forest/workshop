module Infra.Web.Handler.Task where

import Data.Aeson (FromJSON)
import Domain.Todo.TaskRepo (TaskRepo)
import qualified Domain.Todo.UseCase as TaskUseCase
import Infra.Web.Error (Error (..))
import Network.HTTP.Types (status400)
import Relude
import Web.Scotty.Trans (ActionT, json, jsonData, status)

data CreateTaskInput = CreateTaskInput
  { content :: Text
  }
  deriving (Show, Generic)

instance FromJSON CreateTaskInput

createTask :: (MonadIO m, TaskRepo m) => ActionT m ()
createTask = do
  input <- jsonData
  result <- lift $ TaskUseCase.createTask (content input)
  case result of
    Left msg -> do
      status status400
      json $ Error msg
    Right task -> json task