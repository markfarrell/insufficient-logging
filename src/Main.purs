module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)

import UDP as UDP

main :: Effect Unit
main = do
  server <- UDP.createSocket
  _ <- UDP.bind server 4000
  _ <- UDP.onMessage log $ server
  client <- UDP.createSocket
  _ <- UDP.send client "127.0.0.1" 4000 "Hello World!"  
  pure unit
