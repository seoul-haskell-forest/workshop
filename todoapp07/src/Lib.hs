{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib
  ( someFunc,
  )
where

import Database.PostgreSQL.Simple (Connection)
import Domain.Todo.TaskRepo (TaskRepo (..))
import qualified Domain.Todo.UseCase as TaskUseCase
import Infra.PgDatabase.Connection (withConnection)
import qualified Infra.PgDatabase.Task as PgTask
import Network.Wai (Response)
import Relude hiding (get)
import UnliftIO (MonadUnliftIO)
import Web.Scotty.Trans

newtype App a
  = App {unApp :: ReaderT Connection IO a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Connection, MonadUnliftIO)

instance TaskRepo App where
  save task = do
    conn <- ask
    liftIO $ PgTask.insertTask conn task

someFunc :: IO ()
someFunc = main

runner :: Connection -> App Response -> IO Response
runner conn app = runReaderT (unApp app) conn

main :: IO ()
main = do
  withConnection $ \conn -> do
    scottyT 3000 (runner conn) $ do
      post "/tasks" $ do
        result <- lift $ TaskUseCase.createTask "Hello"
        case result of
          Left err -> json err
          Right task -> json True
