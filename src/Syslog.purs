module Syslog
  ( Facility(..)
  , Severity(..)
  , log
  ) where

import Prelude
import Effect.Aff (Aff)
import Effect.Aff.Compat(EffectFnAff, fromEffectFnAff)

data Facility = Audit

data Severity = Debug | Info | Notice

facility' :: Facility -> Int
facility' Audit = 13

severity' :: Severity -> Int
severity' Debug = 7
severity' Info = 6
severity' Notice = 5

foreign import logImpl :: Int -> Int -> String -> String -> EffectFnAff Unit

log :: Facility -> Severity -> String -> String -> Aff Unit
log facility severity msgid  msg = fromEffectFnAff $ logImpl (severity' severity) (facility' facility) msgid msg
