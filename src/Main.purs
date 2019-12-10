module Main where 
  
import Prelude

import Control.Alt ((<|>))
import Control.Coroutine (Producer, Consumer, Process, pullFrom, await, runProcess)
import Control.Coroutine.Aff (produce, emit)
import Control.Monad.Error.Class (try)
import Control.Monad.Rec.Class (forever)
import Control.Monad.Trans.Class (lift)

import Data.Either(Either(..))
import Data.Tuple(Tuple(..))

import Effect (Effect)
import Effect.Aff (Aff, launchAff)
import Effect.Console (log) as Console
import Effect.Class (liftEffect)

import Data.Traversable(foldMap)
import Data.List(many)
import Data.String.CodeUnits (singleton)

import Text.Parsing.Parser (Parser, runParser)
import Text.Parsing.Parser.String (string, anyChar)

import DB as DB
import HTTP as HTTP
import SQLite3 as SQLite3

data MessageType = Success | Failure

data MessageID = DatabaseRequest | RouteRequest | RouteResponse

data Message = Message MessageType MessageID String

instance showMessageType :: Show MessageType where
  show Success = "SUCCESS"
  show Failure = "FAILURE"

instance showMessageID :: Show MessageID where
  show DatabaseRequest = "DATABASE-REQUEST"
  show RouteRequest    = "ROUTE-REQUEST"
  show RouteResponse   = "ROUTE-RESPONSE"

instance showMessage :: Show Message where
  show (Message ty id msg) = "Message " <> show ty <> " " <> show id <> " " <> show msg  

audit :: Message -> Aff Unit
audit = liftEffect <<< Console.log <<< show
  
insertMessage :: String -> Message -> DB.Request Unit
insertMessage filename (Message ty id msg) = do
  database <- DB.connect filename SQLite3.OpenReadWrite
  _ <- DB.all query database
  _ <- DB.close database
  lift $ pure unit
  where query = "INSERT INTO Messages(type,id,msg) VALUES ('" <> show ty <> "','" <> show id  <> "','" <> msg <> "')" 

data Route = InsertMessage Message
 
instance showRoute :: Show Route where
  show (InsertMessage message) = "InsertMessage (" <> show message <> ")"

parseMessageType :: Parser String MessageType
parseMessageType = do
  _ <- string "type"
  _ <- string "="
  parseSuccess <|> parseFailure
  where 
    parseSuccess = string (show Success) >>= const (pure Success)
    parseFailure = string (show Failure) >>= const (pure Failure)

parseMessageID :: Parser String MessageID
parseMessageID = do
  _ <- string "id"
  _ <- string "="
  parseDatabaseRequest <|> parseRouteRequest <|> parseRouteResponse
  where
    parseDatabaseRequest = string (show DatabaseRequest) >>= const (pure DatabaseRequest)
    parseRouteRequest = string (show RouteRequest) >>= const (pure RouteRequest)
    parseRouteResponse = string (show RouteResponse) >>= const (pure RouteResponse)

parseMessageString :: Parser String String
parseMessageString = do
  _ <- string "msg"
  _ <- string "="
  foldMap singleton <$> many anyChar

parseMessageQuery :: Parser String Message
parseMessageQuery = do
  ty <- parseMessageType
  _ <- string "&"
  id <- parseMessageID
  _ <- string "&"
  msg <- parseMessageString
  pure $ Message ty id msg

parseInsertMessage :: Parser String Route
parseInsertMessage = do
  _ <- string "/insert/message"
  _ <- string "?"
  message <- parseMessageQuery
  pure (InsertMessage message)

parseRoute :: Parser String Route
parseRoute = parseInsertMessage

data ContentType a = TextHTML a

data ResponseType a = Ok (ContentType a) | InternalServerError a | BadRequest String

instance showContentType :: (Show a) => Show (ContentType a) where
  show (TextHTML x) = "TextHTML (" <> show x <> ")"

instance showResponseType :: (Show a) => Show (ResponseType a) where
  show (Ok x)                  = "Ok (" <> show x <> ")"
  show (InternalServerError x) = "InternalServerError (" <> show x <> ")"
  show (BadRequest path)       = "BadRequest " <> show path

runRoute :: HTTP.IncomingMessage -> Aff (ResponseType Route)
runRoute req  = do
  result <- pure $ flip runParser parseRoute $ HTTP.messageURL req
  case result of
    (Left _)                        -> pure $ BadRequest (HTTP.messageURL req)
    (Right (InsertMessage message)) -> do
      result' <- DB.runRequest $ insertMessage filename message
      case result' of
        (Left error)             -> do 
           _ <- audit $ Message Failure DatabaseRequest (show error) 
           pure $ InternalServerError (InsertMessage message)
        (Right (Tuple _ steps)) -> do
           _ <- audit $ Message Success DatabaseRequest (show steps) 
           pure $ Ok (TextHTML (InsertMessage message))
  where filename = "log.db"
 
respondRoute :: ResponseType Route -> HTTP.ServerResponse -> Aff Unit
respondRoute (Ok (TextHTML (InsertMessage message))) = \res -> liftEffect $ do
  _ <- HTTP.setHeader "Content-Type" "text/html" $ res
  _ <- HTTP.writeHead 200 $ res
  _ <- HTTP.write body $ res
  _ <- HTTP.end $ res
  pure unit
  where body = show (InsertMessage message)
respondRoute (BadRequest _) = \res -> liftEffect $ do
  _ <- HTTP.writeHead 400 $ res
  _ <- HTTP.end $ res
  pure unit  
respondRoute (InternalServerError _) = \res -> liftEffect $ do
  _ <- HTTP.writeHead 500 $ res
  _ <- HTTP.end $ res
  pure unit

producer :: HTTP.Server -> Producer HTTP.Request Aff Unit
producer server = produce \emitter -> do
  HTTP.onRequest (\req res -> emit emitter $ HTTP.Request req res) $ server

consumer :: Consumer HTTP.Request Aff Unit
consumer = forever $ do
  request <- await
  case request of
    (HTTP.Request req res) -> do
      routeResult <- lift $ try (runRoute req)
      case routeResult of
        (Left  error)        -> lift $ audit $ Message Failure RouteRequest (show error)
        (Right responseType) -> do
           _ <- lift $ audit $ Message Success RouteRequest (HTTP.messageURL req)
           responseResult <- lift $ try (respondRoute responseType res)
           case responseResult of
             (Left error')   -> lift $ audit $ Message Failure RouteResponse (show error')
             (Right _)       -> lift $ audit $ Message Success RouteResponse (show responseType) 

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
main = void $ launchServer 3000
