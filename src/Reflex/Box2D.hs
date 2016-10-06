{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE JavaScriptFFI     #-}
{-# LANGUAGE OverloadedStrings #-}

module Reflex.Box2D where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Default
import Reflex.Dom
import Reflex.Box2D.Foreign
import GHC.Generics
import Reflex.Box2D.Types

data WorldConfig t = WorldConfig {
    world_initialGravity   :: Vec2
  , world_setGravity       :: Event t Vec2
  , world_initialSleepable :: Bool
  , world_setSleepable     :: Event t Bool
  }

instance Reflex t => Default (WorldConfig t) where
  def = WorldConfig (Vec2 0 10) never True never

data World = World WorldToken

type SupportsReflexBox2D t m = (Reflex t, MonadIO m, PerformEvent t m, MonadIO (Performable m))

world :: SupportsReflexBox2D t m => WorldConfig t -> m World
world (WorldConfig g0 dG s0 dS) = do
  w <- liftIO $ makeWorld g0 s0
  performEvent_ (ffor dG $ liftIO . worldSetGravity w)
  return $ World w

