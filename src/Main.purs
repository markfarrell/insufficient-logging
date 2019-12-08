module Main where

import Prelude

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Aff (Aff, launchAff)

import Data.Either (Either(..))
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), fst, snd)

import Control.Coroutine (Producer, Consumer, Process, pullFrom, await, runProcess)
import Control.Coroutine.Aff (produce, emit)
import Control.Monad.Rec.Class (forever)
import Control.Monad.Trans.Class (lift)

import DB as DB
import HTTP as HTTP
import SQLite3 as SQLite3
import URL as URL

respondOK :: String -> HTTP.ServerResponse -> Effect Unit
respondOK body res = do
  _ <- HTTP.setHeader "Content-Type" "text/json" $ res
  _ <- HTTP.writeHead 200 $ res
  _ <- HTTP.write body $ res
  _ <- HTTP.end $ res
  pure unit

respondBadRequest :: HTTP.ServerResponse -> Effect Unit
respondBadRequest res = do
  _ <- HTTP.writeHead 400 $ res
  _ <- HTTP.end $ res
  pure unit

producer :: HTTP.Server -> Producer (Tuple HTTP.IncomingMessage HTTP.ServerResponse) Aff Unit
producer server = produce \emitter -> do
  HTTP.onRequest (\req res -> emit emitter $ Tuple req res) $ server

consumer :: Consumer (Tuple HTTP.IncomingMessage HTTP.ServerResponse) Aff Unit
consumer = forever $ do
  request <- await
  result <-  lift <<< liftEffect $ (URL.runMatch "/" "q" <<< HTTP.messageURL $ fst request)
  result' <- lift $ join <$> (sequence $ runQuery' <$> result)
  case result' of
    (Left  _) -> lift <<< liftEffect $ respondBadRequest (snd request)
    (Right rows) -> do lift <<< liftEffect $ respondOK (show rows) (snd request)
  where runQuery' pattern = DB.runQuery "chinook.db" SQLite3.OpenReadOnly $
          "SELECT * FROM Track WHERE name LIKE '%" <> pattern <> "%'"

process :: HTTP.Server -> Process Aff Unit
process server = pullFrom consumer $ producer server

launchProcess :: HTTP.Server -> Effect Unit
launchProcess server = void $ launchAff $ do
  runProcess $ process server

launchServer :: Int -> Effect Unit
launchServer port = do
  server <- HTTP.createServer
  _ <- launchProcess $ server
  _ <- HTTP.listen port $ server
  pure unit

main :: Effect Unit
main = launchServer 3000
