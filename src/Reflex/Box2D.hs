{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE JavaScriptFFI     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE TypeFamilies      #-}

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
import GHCJS.DOM.Types (HTMLCanvasElement)
import Reflex.Box2D.Types

-------------------------------------------------------------------------------
type SupportsReflexBox2D t m = (Reflex t, MonadIO m, PerformEvent t m,
                                MonadIO (Performable m), MonadFix m,
                                PostBuild t m, TriggerEvent t m,
                                MonadHold t m, DomBuilder t m, DomBuilderSpace m ~ GhcjsDomSpace )


-------------------------------------------------------------------------------
world :: forall t m.SupportsReflexBox2D t m => Maybe HTMLCanvasElement -> WorldConfig t -> m (World t)
world canv WorldConfig{..} = do
  w <- liftIO $ makeWorld worldConfig_initialGravity
                          worldConfig_initialSleepable
  maybe (return ()) (liftIO . drawSetup w) canv
  performEvent_ (ffor worldConfig_setGravity $ liftIO . worldSetGravity w)

  let mkTicks ClockNever g = gate g <$> return never
      mkTicks (ClockRegularFrequency freq) g = gate g . (() <$) <$> do
        liftIO getCurrentTime >>= tickLossy (1 / realToFrac freq)
  (physicsTs, drawTs) <- liftM2 (,)
    (mkTicks worldConfig_physicsTicks worldConfig_physicsRunning)
    (mkTicks worldConfig_drawTicks worldConfig_drawRunning)


  bodies <- listWithKeyShallowDiff
    worldConfig_initialBodies worldConfig_setBodies $ \k v dv ->
      widgetHold (body drawTs w v) (body drawTs w <$> dv)

  let wrld = World w (joinDynThroughMap bodies) physicsTs drawTs

  performEvent $ (liftIO $ worldStep w (1/60) 10 10) <$ physicsTs
  -- TODO use appropriate timestep. 1/60 is only sometimes right by accident

  -- liftIO $ drawSetup w
  performEvent $ (liftIO $ drawDebugData w) <$ drawTs
  -- performEvent $ (liftIO $ putStrLn "draw") <$ drawTs
  -- performEvent $ (liftIO $ putStrLn "phys") <$ physicsTs

  return $ wrld -- World w (joinDynThroughMap bodies) physicsTs drawTs

-------------------------------------------------------------------------------
body :: SupportsReflexBox2D t m => Event t () -> WorldToken -> BodyConfig t -> m (Body t)
body sampleTicks w (BodyConfig bType (p0,a0) dp f0 df) = do
  b <- liftIO $ createBody w (BodyDef bType p0 a0)
  -- liftIO $ bodySetType b bType >> bodySetX b x0 >> bodySetY b y0
  -- liftIO $ bodySetType b bType >> bodySetPositionAndAngle b p0 a0
  pForced <- performEvent $ ffor dp $ \f -> liftIO $ do
    v <- bodyGetPosition b
    rot <- return 0 -- TODO: get rotation
    let (p',r') = f (v,rot)
    bodySetPositionAndAngle b p' r'
    return (p',r')

  pSimulation <- performEvent $ ffor sampleTicks $ \() -> liftIO $ do
    p <- bodyGetPosition b
    rot <- return 0 -- TODO: get rotation
    return (p, rot)

  p <- holdDyn (p0,a0) $ leftmost [pForced, pSimulation]
  -- p <- setAndTrack p0 dp
  --      (\(Vec2 x y) -> bodySetX b x >> bodySetY b y)
  --      ((liftA2 Vec2 (bodyGetX b) (bodyGetY b)) <$ sampleTicks)

  -- fixturesConfig <- foldDyn (applyMap) f0 df
  fixtures <- listWithKeyShallowDiff f0 df $ \k v dv -> widgetHold (fixture w b v) (fixture w b <$> dv)
  let contacts = constDyn ()

  return $ Body bType p b (joinDynThroughMap fixtures) contacts

data WorldConfig t = WorldConfig {
    worldConfig_initialGravity   :: Vec2
  , worldConfig_setGravity       :: Event t Vec2
  , worldConfig_initialSleepable :: Bool
  , worldConfig_setSleepable     :: Event t Bool
  , worldConfig_initialBodies    :: Map T.Text (BodyConfig t)
  , worldConfig_setBodies        :: Event t (Map T.Text (Maybe (BodyConfig t)))
  , worldConfig_physicsTicks     :: ClockSource
  , worldConfig_physicsRunning   :: Behavior t Bool
  , worldConfig_drawTicks        :: ClockSource
  , worldConfig_drawRunning      :: Behavior t Bool
  }

instance Reflex t => Default (WorldConfig t) where
  def = WorldConfig (Vec2 0 10) never True never mempty never
        (ClockRegularFrequency 600) (constant True)
        (ClockRegularFrequency 60)  (constant True)

data World t = World {
    world_token         :: WorldToken
  , world_bodies        :: Dynamic t (Map T.Text (Body t))
  , world_physics_ticks :: Event t()
  , world_draw_ticks    :: Event t ()
  }

data FixtureConfig t = FixtureConfig
  { fixtureConfig_initialDensity     :: Double
  , fixtureConfig_setDensity         :: Event t Double
  , fixtureConfig_initialFriction    :: Double
  , fixtureConfig_setFriction        :: Event t Double
  , fixtureConfig_initialRestitution :: Double
  , fixtureConfig_setRestitution     :: Event t Double
  , fixtureConfig_initialShape       :: Shape
  , fixtureConfig_setShape           :: Event t Shape
  }

instance Reflex t => Default (FixtureConfig t) where
  def = FixtureConfig 1 never 0.5 never 0.2 never (ShapeCircle 1) never

data Fixture t = Fixture {
    fixture_density     :: Dynamic t Double
  , fixture_friction    :: Dynamic t Double
  , fixture_restitution :: Dynamic t Double
  , fixture_shape       :: Dynamic t Shape
  , fixture_token       :: FixtureToken
  }

data BodyConfig t = BodyConfig
  { bodyConfig_bodyType :: BodyType
  , bodyConfig_initialPosition :: (Vec2, Double)
  , bodyConfig_modifyPosition :: Event t ((Vec2, Double) -> (Vec2, Double))
  , bodyConfig_initialFixtures :: Map T.Text (FixtureConfig t)
  , bodyConfig_setFixtures :: Event t (Map T.Text (Maybe (FixtureConfig t)))
  }

instance Reflex t => Default (BodyConfig t) where
  def = BodyConfig BodyTypeDynamic (Vec2 5 5, 0) never mempty never


data Body t = Body {
    body_type      :: BodyType
  , body_position  :: Dynamic t (Vec2, Double)
  , body_token     :: BodyToken
  , body_fixtures  :: Dynamic t (Map T.Text (Fixture t))
  , body_contacts  :: Dynamic t () -- TODO
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

fixture :: SupportsReflexBox2D t m => WorldToken -> BodyToken -> FixtureConfig t -> m (Fixture t)
fixture w b (FixtureConfig d0 dd f0 df r0 dr s0 ds) = do
  f <- liftIO $ makeFixture b (FixtureDef d0 f0 r0 s0)
  -- density <- setAndTrack d0 dd (fixtureSetDensity f) never
  -- friction <- setAndTrack f0 df (fixtureSetFriction f) never
  -- shape <- setAndTrack s0 ds (fixtureSetShape f) never
  -- restitution <- setAndTrack r0 dr (fixtureSetRestitution f) never
  -- liftIO $ worldCreateBodyAndFixture w b f
  density <- return $ constDyn d0 -- TODO: Track these
  friction <- return $ constDyn f0
  restitution <- return $ constDyn r0
  shape <- return $ constDyn s0
  return $ Fixture density friction restitution shape f
