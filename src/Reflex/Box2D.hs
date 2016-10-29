{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE JavaScriptFFI     #-}
{-# LANGUAGE OverloadedStrings #-}

module Reflex.Box2D where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Default
import Data.Map (Map)
import qualified Data.Text as T
import Reflex.Dom
import Reflex.Box2D.Foreign
import GHC.Generics
import Reflex.Box2D.Types

type SupportsReflexBox2D t m = (Reflex t, MonadIO m, PerformEvent t m,
                                MonadIO (Performable m))


world :: SupportsReflexBox2D t m => WorldConfig t -> m (World t)
world (WorldConfig g0 dG s0 dS) = do
  w <- liftIO $ makeWorld g0 s0
  performEvent_ (ffor dG $ liftIO . worldSetGravity w)
  return $ World w 

body :: SupportsReflexBox2D t m => World t -> BodyDefConfig t -> m (BodyDef t)
body w (BodyDefConfig bType p0 dp f0 df) = do
  b <- liftIO makeBodyDef
  liftIO $ bodyDefSetType b bType >> bodyDefSetX b (x p0)
                                  >> bodyDefSetY b (y p0)
  pSim <- performEvent $ ffor (world_draw_ticks w) $ \() -> do
    liftA2 Vec2 (bodyDefGetX b) (bodyDefGetY b)
  p <- holdDyn p0 $ leftmost [dp ] -- TODO
  fixtures0 <- liftIO $ ffor f0 $ \fixture -> do
    worldCreateBodyAndFixture w b
  fixtures <- holdDyn fixtures0 never -- TODO: add/delete fixtures
  return $ BodyDef bType 

data WorldConfig t = WorldConfig {
    worldConfig_initialGravity   :: Vec2
  , worldConfig_setGravity       :: Event t Vec2
  , worldConfig_initialSleepable :: Bool
  , worldConfig_setSleepable     :: Event t Bool
  , worldConfig_initialBodies    :: Map T.Text (BodyDef t)
  , worldConfig_setBodies        :: Event t (Map T.Text (Maybe (BodyDef t)))
  , worldConfig_physics_ticks    :: ClockSource
  , worldConfig_draw_ticks             :: ClockSource
  }

instance Reflex t => Default (WorldConfig t) where
  def = WorldConfig (Vec2 0 10) never True never
    (ClockRegularFrequency 600) (ClockRegularFrequency 60)

data World t = World {
    world_token         :: WorldToken
  , world_physics_ticks :: Event t()
  , world_draw_ticks    :: Event t ()
  }

data FixtureDefConfig t = FixtureDefConfig
  { fixtureDefConfig_initialDensity :: Double
  , fixtureDefConfig_setDensity     :: Event t Double
  , fixtureDefConfig_initialShape   :: Shape
  , fixtureDefConfig_setShape       :: Event t Shape
  }

instance Reflex t => Default (FixtureDefConfig t) where
  def = FixtureDefConfig 1 never (ShapeCircle 1) never

data FixtureDef t = FixtureDef {
    fixtureDef_density     :: Dynamic t Double
  , fixtureDef_friction    :: Dynamic t Double
  , fixtureDef_restitution :: Dynamic t Double
  , fixtureDef_shape       :: Shape
  , fixtureDef_token       :: FixtureToken
  }

data BodyDefConfig t = BodyDefConfig
  { bodyDefConfig_bodyType :: BodyType
  , bodyDefConfig_initialPosition :: Vec2
  , bodyDefConfig_setPosition :: Event t Vec2
  , bodyDefConfig_initialFixtures :: Map T.Text (FixtureDef t)
  , bodyDefConfig_setFixtures :: Map T.Text (Maybe (FixtureDef t))
  }

instance Reflex t => Default (BodyDefConfig t) where
  def = BodyDefConfig BodyTypeDynamic (Vec2 5 5) never mempty never


data BodyDef t = BodyDef {
    bodyDef_type      :: BodyType
  , bodyDef_position  :: Dynamic t Vec2
  , bodyDef_token     :: BodyToken
  , bodyDef_fixtures  :: Dynamic t (Map T.Text (FixtureDef t))
  , bodyDef_contacts  :: Dynamic t () -- TODO
  }
