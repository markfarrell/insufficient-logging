module DB
 ( Request
 , RequestDSL
 , close
 , connect
 , all 
 , runRequest
 , runQuery
 ) where

import Prelude

import Control.Monad.Free.Trans (FreeT, liftFreeT, runFreeT)
import Control.Monad.Error.Class (try)
import Control.Monad.Trans.Class (lift)

import Data.Either (Either(..))

import Effect.Aff (Aff)
import Effect.Exception (Error)

import SQLite3 as SQLite3
import Syslog (Facility(..), Severity(..), log)

data RequestDSL a = Close SQLite3.Database (Unit -> a)
  | Connect String SQLite3.Mode (SQLite3.Database -> a) 
  | All String SQLite3.Database (Array SQLite3.Row -> a)

instance functorRequestDSL :: Functor RequestDSL where
  map :: forall a b. (a -> b) -> RequestDSL a -> RequestDSL b
  map f (Close database next)        = (Close database (f <<< next))
  map f (Connect filename mode next) = (Connect filename mode (f <<< next))
  map f (All query database next) = (All query database (f <<< next))

type Request a = FreeT RequestDSL Aff a

close :: SQLite3.Database -> Request Unit
close database = liftFreeT $ (Close database identity)

connect :: String -> SQLite3.Mode -> Request SQLite3.Database
connect filename mode = liftFreeT $ (Connect filename mode identity)

all :: String -> SQLite3.Database -> Request (Array SQLite3.Row)
all query database = liftFreeT $ (All query database identity)

runInterpret :: forall a. RequestDSL (Request a) -> Aff (Request a)
runInterpret (Close database next) = do 
  result <- SQLite3.close database
  pure $ next result
runInterpret (Connect filename mode next) = do
  result <- SQLite3.connect filename mode
  pure $ next result 
runInterpret (All query database next) = do
  result <- SQLite3.all query database
  pure $ next result
 
runRequest ::  forall a. Show a => Request a -> Aff (Either Error a)
runRequest request = do
  result <- try $ runFreeT runInterpret request
  pure result

interpretQuery :: String -> SQLite3.Mode -> String -> Request (Array SQLite3.Row)
interpretQuery filename mode query = do
  database <- connect filename mode 
  rows     <- all query database
  _        <- close database
  lift $ pure rows

runQuery :: String -> SQLite3.Mode -> String -> Aff (Either Error (Array SQLite3.Row))
runQuery filename mode query = runRequest $ interpretQuery filename mode query
  
