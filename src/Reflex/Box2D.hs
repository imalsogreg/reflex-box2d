{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE JavaScriptFFI     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecursiveDo       #-}

module Reflex.Box2D where

import Control.Applicative (liftA2)
import Control.Monad (liftM2)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Fix (MonadFix)
import Data.Default
import Data.Time
import Data.Map (Map)
import qualified Data.Text as T
import Reflex.Dom
import Reflex.Box2D.Foreign
import GHC.Generics
import Reflex.Box2D.Types

type SupportsReflexBox2D t m = (Reflex t, MonadIO m, PerformEvent t m,
                                MonadIO (Performable m), MonadFix m,
                                PostBuild t m, TriggerEvent t m,
                                MonadHold t m)


world :: forall t m.SupportsReflexBox2D t m => WorldConfig t -> m (World t)
world (WorldConfig g0 dG s0 dS bods0 dBods clockPhys clockDraw) = do
  w <- liftIO $ makeWorld g0 s0
  let mkTicks ClockNever = return never
      mkTicks (ClockRegularFrequency freq) = (() <$) <$> do
        liftIO getCurrentTime >>= tickLossy (realToFrac freq)
  (physicsTs, drawTs) <- liftM2 (,) (mkTicks clockPhys) (mkTicks clockDraw)
  performEvent_ (ffor dG $ liftIO . worldSetGravity w)
  return $ World w physicsTs drawTs

-------------------------------------------------------------------------------
body :: SupportsReflexBox2D t m => World t -> BodyDefConfig t -> m (BodyDef t)
body w (BodyDefConfig bType p0 dp f0 df) = do
  b <- liftIO makeBodyDef
  liftIO $ bodyDefSetType b bType

  p <- setAndTrack p0 dp
       (\(Vec2 x y) -> bodyDefSetX b x >> bodyDefSetY b y)
       ((liftA2 Vec2 (bodyDefGetX b) (bodyDefGetY b)) <$ world_draw_ticks w)

  fixtures <- foldDyn (applyMap) f0 df
  let contacts = constDyn ()
  return $ BodyDef bType p b fixtures contacts

data WorldConfig t = WorldConfig {
    worldConfig_initialGravity   :: Vec2
  , worldConfig_setGravity       :: Event t Vec2
  , worldConfig_initialSleepable :: Bool
  , worldConfig_setSleepable     :: Event t Bool
  , worldConfig_initialBodies    :: Map T.Text (BodyDef t)
  , worldConfig_setBodies        :: Event t (Map T.Text (Maybe (BodyDef t)))
  , worldConfig_physics_ticks    :: ClockSource
  , worldConfig_draw_ticks       :: ClockSource
  }

instance Reflex t => Default (WorldConfig t) where
  def = WorldConfig (Vec2 0 10) never True never mempty never (ClockRegularFrequency 600) (ClockRegularFrequency 60)

data World t = World {
    world_token         :: WorldToken
  , world_physics_ticks :: Event t()
  , world_draw_ticks    :: Event t ()
  }

data FixtureDefConfig t = FixtureDefConfig
  { fixtureDefConfig_initialDensity     :: Double
  , fixtureDefConfig_setDensity         :: Event t Double
  , fixtureDefConfig_initialFriction    :: Double
  , fixtureDefConfig_setFriction        :: Event t Double
  , fixtureDefConfig_initialRestitution :: Double
  , fixtureDefConfig_setRestitution     :: Event t Double
  , fixtureDefConfig_initialShape       :: Shape
  , fixtureDefConfig_setShape           :: Event t Shape
  }

instance Reflex t => Default (FixtureDefConfig t) where
  def = FixtureDefConfig 1 never 0.5 never 0.2 never (ShapeCircle 1) never

data FixtureDef t = FixtureDef {
    fixtureDef_density     :: Dynamic t Double
  , fixtureDef_friction    :: Dynamic t Double
  , fixtureDef_restitution :: Dynamic t Double
  , fixtureDef_shape       :: Dynamic t Shape
  , fixtureDef_token       :: FixtureToken
  }

data BodyDefConfig t = BodyDefConfig
  { bodyDefConfig_bodyType :: BodyType
  , bodyDefConfig_initialPosition :: Vec2
  , bodyDefConfig_setPosition :: Event t Vec2
  , bodyDefConfig_initialFixtures :: Map T.Text (FixtureDef t)
  , bodyDefConfig_setFixtures :: Event t (Map T.Text (Maybe (FixtureDef t)))
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

setAndTrack
  :: SupportsReflexBox2D t m
  => a
  -> Event t a
  -> (a -> IO ())
  -> Event t (IO a)
  -> m (Dynamic t a)
setAndTrack a0 da act probe = do
  liftIO $ act a0
  performEvent $ liftIO . act <$> da
  simulationUpdates <- performEvent (liftIO <$> probe)
  holdDyn a0 $ leftmost [da, simulationUpdates]

fixture :: SupportsReflexBox2D t m => FixtureDefConfig t -> m (FixtureDef t)
fixture (FixtureDefConfig d0 dd f0 df r0 dr s0 ds) = do
  f <- liftIO makeFixture
  density <- setAndTrack d0 dd (fixtureDefSetDensity f) never
  friction <- setAndTrack f0 df (fixtureDefSetFriction f) never
  shape <- setAndTrack s0 ds (fixtureDefSetShape f) never
  restitution <- setAndTrack r0 dr (fixtureDefSetRestitution f) never
  return $ FixtureDef density friction restitution shape f
