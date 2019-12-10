module Main where

import Prelude

import Control.Monad.Trans.Class (lift)
import Data.Either(Either(..))

import Effect (Effect)
import Effect.Aff (Aff, launchAff)
import Effect.Console (log) as Console
import Effect.Class (liftEffect)

import DB as DB
import SQLite3 as SQLite3

data Severity = Success | Failure

data Message = Message Severity String

instance showSeverity :: Show Severity where
  show Success = "SUCCESS"
  show Failure = "FAILURE"

log :: String -> Aff Unit
log = liftEffect <<< Console.log

insertMessage :: SQLite3.Database -> Message -> DB.Request Unit
insertMessage database (Message severity msg) = do
  _ <- DB.all query database
  lift $ pure unit
  where query = "INSERT INTO Messages (severity, msg) VALUES (" <> show severity <> ", " <> msg <> ")"

createMessages :: SQLite3.Database -> DB.Request Unit
createMessages database = do
  _ <- DB.all query database
  lift $ pure unit
  where query = "CREATE TABLE Messages (severity TEXT NOT NULL, msg TEXT NOT NULL)"

openReadWrite :: String -> DB.Request SQLite3.Database
openReadWrite filename = do
  database <- DB.connect filename SQLite3.OpenReadWrite
  lift $ pure database

initialize :: String -> DB.Request SQLite3.Database
initialize filename = do
  database <- openReadWrite filename
  _ <- createMessages database
  lift $ pure database

main :: Effect Unit
main = void $ launchAff $ do
  result <- DB.runRequest $ initialize ":memory:"
  log $ show result
