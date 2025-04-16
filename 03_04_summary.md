# 3일차, 4일차 요약

## Type Class

### Type Class

```haskell
data Point = Point Float Float

class YesNo a where
  yesno :: a -> Bool

instance YesNo Int where
  yesno 0 = False
  yesno _ = True

foo :: YesNo a => a -> Bool
foo x = yesno x
```

### Functor

```haskell
class Functor f where
	fmap :: (a -> b) -> f a -> f b 
  (<$>) :: (a -> b) -> f a -> f b
```

* 어떤 함수와 펑터 타입을 받아 함수를 적용한다.
* `fmap`보다 중위 함수인 `(<$>)`를 더 많이 쓴다.

### Monad

```haskell
class Applicative m => Monad m where
  (>>=) :: m a -> (a -> m b) -> m b
  return :: a -> m a
```

* 모나드 값과 함수를 받아 함수를 적용한 모나드 값을 리턴한다. 인자 함수는 모나드 값을 리턴해야 한다.
* 안 쪽 함수에 다시 `(>>=)`를 써서 모나드를 연속해서 쓸 수 있다.
* `return`은 일반 값을 모나드 값으로 만든다. 모든 모나드는 어플리커티브이기 때문에 `pure`를 더 많이 쓴다.
* 모나드를 쉽게 쓸 수 있는 `do` 구문이 있다.
  ```haskell
  app = do
    심볼 <- 모나드 값
    ...
    심볼 <- 모나드 값
    모나드 값
  ```

## Type

### Maybe 

```haskell
data Maybe a = Just a | Nothing
```

* 계산 결과 값이 없을 때를 표현할 때 주로 쓴다.
* 모나드 인스턴스가 있다.

```haskell
phones :: [(String, String)]
phones = [("123-123-123", "Todd"), ("000-000-000", "Eunmin")]

ids :: [(String, Int)]
ids = [("Todd", 32), ("Eunmin", 424)]

ages :: [(Int, Int)]
ages = [(32, 100), (42, 50)]

getAgeByPhone :: String -> Maybe Int
getAgeByPhone phone = do
  name <- lookup phone phones
  id <- lookup name ids
  age <- lookup id ages
  return age
```

### Either

```haskell
data Either a b = Left a | Right b
```

* 왼쪽에 에러 타입을 오른쪽에 결과 타입으로 계산에 실패 한 경우 왼쪽에 실패 내용을 담아 준다.
* 모나드 인스턴스가 있다.


```haskell
import Data.Aeson (eitherDecode)

eitherDecode :: String -> Either String a
```

```haskell
value :: Either String Int
value = do
  x <- eitherDecode "1"
  y <- eitherDecode "2"
  pure $ x + y
```

### Reader

* 주로 모나드 맥락 안에서 외부 값을 읽어 올 때 사용한다.
* 모나드 인스턴스가 있다.

```haskell
import Control.Monad.Reader (Reader, ask, runReader)

app :: Reader Bool Int
app = do
  let x = 40
  isDevMode <- ask
  let y = if isDevMode then 0 else 2
  pure $ x + y

value :: Int
value = runReader app False
```

### Writer

* 주로 모나드 맥락 안에서 무엇인가를 기록 할 때 사용한다.
* 모나드 인스턴스가 있다.

```haskell
import Control.Monad.Writer (Writer, tell, runWriter)

app :: Writer [String] Int
app = do
    let x = 40
    let y = 2
    tell [ "y = " <> show y ]
    return $ x + y

value :: (Int, [String])
value = runWriter app
```

### State

* 주로 모나드 맥락 안에서 변수처럼 어떤 값을 쓰거나 읽을 때 사용한다.
* 모나드 인스턴스가 있다.

```haskell
import Control.Monad.State (Writer, get, put, runState)

app :: State Int Int
app = do
  let x = 40
  value <- get
  put $ value + 1
  value <- get
  put $ value + 1
  value <- get
  return $ x + value

value :: (Int, Int)
value = runState app 0
```

### IO

* 모나드 맥락 안에서 `IO`를 쌓고 나중에 한 번에 실행한다.
* 실행기에 따라 다르게 동작할 수 있기 때문에 표현과 실행을 분리해준다.

```haskell
app :: IO Int
app = do
  x <- readLn
  y <- readLn
  pure $ x + y
```

## MonadTransformer

* 여러 개의 모나드 효과를 하나의 모나드로 합쳐서 사용하기
* `transformers` 패키지에 정의

### ReaderT

* `Reader`와 다른 모나드를 합칠 수 있음

```haskell
data ReaderT r m a = ...
```

* `m`은 다른 모나드
* 안 쪽 모나드를 바깥 쪽 모나드로 바꾸기 위해 `lift`

```haskell
class MonadTrans t where
  lift :: Monad m => m a -> t m a
```

```haskell
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)

app :: ReaderT Int IO Int
app = do
  x <- ask
  y <- lift readLn
  pure $ x + y

main :: IO ()
main = do
  result <- runReaderT app 40
  print result
```

### 하나 더 쌓기 

* 한 칸 올리려면 `lift`, 두 칸 올리려면 `lift $ lift`

```haskell
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Trans.Writer (WriterT, runWriterT, tell)

app :: ReaderT Int (WriterT String IO) Int
app = do
  x <- ask
  y <- lift $ lift readLn
  lift $ tell $ "y: " <> show y
  pure $ x + y

main :: IO ()
main = do
  let writer = runReaderT app 40
  (result, logs) <- runWriterT writer
  putStrLn logs
  print result
```

* 특별히 `IO` 모나드는 `lift` 중첩 대신 `liftIO`로 한 번에 올릴 수 있다.

```haskell
import Control.Monad.IO.Class (liftIO)
...

app :: ReaderT Int (WriterT String IO) Int
app = do
  x <- ask
  y <- liftIO readLn
  tell $ "y: " <> show y
  pure $ x + y
```

## mtl 스타일

* `mtl` 패키지에 있음 
* 모나드 트랜스포머를 타입 제약으로 풀어 `lift` 없이 여러 모나드 효과를 쓸 수 있음

```haskell
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, ask, runReaderT) -- 모듈이 다르다.
import Control.Monad.Writer (WriterT, runWriterT, tell)

app :: ReaderT Int (WriterT String IO) Int
app = do
  x <- ask
  y <- liftIO readLn
  tell $ "y: " <> show y -- lift를 하지 않는다.
  pure $ x + y

main :: IO ()
main = do
  let writer = runReaderT app 40
  (result, logs) <- runWriterT writer
  putStrLn logs
  print result
```

* mtl 스타일을 타입에 유연하게 쓰기

```haskell
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (MonadLogger, logInfoN, runStdoutLoggingT)
import Control.Monad.Reader (MonadReader, ask, runReaderT)
import Data.Text (pack)

app :: (MonadReader Int m, MonadLogger m, MonadIO m) => m Int
app = do
  x <- ask
  y <- liftIO readLn
  logInfoN $ "y: " <> pack (show y)
  pure $ x + y

main :: IO ()
main = do
  let logger = runReaderT app 40
  result <- runStdoutLoggingT logger
  print result
```

* mtl에서 내가 만든 타입 클래스 쓰기


```haskell
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import Control.Monad.Writer (MonadWriter, WriterT, runWriterT, tell)

class UserRepository m where
  findNameById :: Int -> m String

newtype App a
  = App {unApp :: ReaderT Int (WriterT String IO) a}
  deriving (Functor, Applicative, Monad, MonadReader Int, MonadWriter String, MonadIO)

instance UserRepository App where
  findNameById _ = pure "Eunmin"

app :: (MonadReader Int m, MonadWriter String m, MonadIO m, UserRepository m) => m Int
app = do
  x <- ask
  userId <- liftIO readLn
  userName <- findNameById userId
  tell $ "userId: " <> show userId <> ", userName: " <> userName
  pure $ x + userId

main :: IO ()
main = do
  let writer = runReaderT (unApp app) 40
  (result, logs) <- runWriterT writer
  putStrLn logs
  print result
```