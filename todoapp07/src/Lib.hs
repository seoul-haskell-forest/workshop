{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib
  ( someFunc,
  )
where

import Control.Exception (bracket)
import Data.ULID (ULID)
import Database.PostgreSQL.Simple (ConnectInfo (..), Connection, close, connect, execute, query)
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow (FromRow)
import Database.PostgreSQL.Simple.ToField (ToField (..))
import Database.PostgreSQL.Simple.ToRow (ToRow)
import Domain.Todo.Task (Task (..))
import Domain.Todo.TaskRepo (TaskRepo (..))
import qualified Domain.Todo.UseCase as TaskUseCase
import Relude

newtype App a
  = App {unApp :: ReaderT Connection IO a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Connection)

instance ToRow Task

instance ToField ULID where
  toField ulid = toField (show ulid :: Text)

instance FromRow Task

instance FromField ULID where
  fromField f mbs = fromMaybe (error "ULID parse error") . readMaybe <$> fromField f mbs

instance TaskRepo App where
  save task = do
    conn <- ask
    affectedCount <- liftIO $ execute conn "INSERT INTO tasks (id, content, completed, craeted_at, updated_at) VALUES (?, ?, ?, ?, ?)" task
    (result :: [Task]) <- liftIO $ query conn "SELECT * FROM tasks" ()
    liftIO $ print result
    pure $ affectedCount == 1

main :: IO ()
main = do
  bracket
    ( connect
        $ ConnectInfo
          { connectHost = "localhost",
            connectPort = 5432,
            connectDatabase = "todoapp",
            connectUser = "constacts",
            connectPassword = ""
          }
    )
    close
    $ \conn -> do
      result <- runReaderT (unApp $ TaskUseCase.createTask "Hello") conn
      print result

someFunc :: IO ()
someFunc = main
