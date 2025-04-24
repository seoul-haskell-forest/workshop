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
import Relude

newtype App a
  = App {unApp :: ReaderT Connection IO a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Connection)

instance TaskRepo App where
  save task = do
    conn <- ask
    liftIO $ PgTask.insertTask conn task

main :: IO ()
main = do
  withConnection $ \conn -> do
    result <- runReaderT (unApp $ TaskUseCase.createTask "Hello") conn
    print result

someFunc :: IO ()
someFunc = main
