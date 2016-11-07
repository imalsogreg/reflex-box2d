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
  , world_physics_ticks :: Event t()
  , world_draw_ticks    :: Event t ()
  }


-------------------------------------------------------------------------------
world'
  :: forall t m a.SupportsReflexBox2D t m
  => Maybe HTMLCanvasElement
  -> WorldConfig t
  -> (World t -> m a)
  -> m (World t, a)
world' canv WorldConfig{..} children = do
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

  let wrld = World w physicsTs drawTs

  performEvent $ (liftIO $ worldStep w (1/60) 10 10) <$ physicsTs
  -- TODO use appropriate timestep. 1/60 is only sometimes right by accident

  performEvent $ (liftIO $ drawDebugData w) <$ drawTs

  a <- children wrld

  return (wrld, a)

-------------------------------------------------------------------------------
world
  :: forall t m a.SupportsReflexBox2D t m
  => Maybe HTMLCanvasElement
  -> WorldConfig t
  -> (World t -> m a)
  -> m a
world canv wConf children = snd <$> world' canv wConf children


-------------------------------------------------------------------------------
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

-------------------------------------------------------------------------------
data Fixture t = Fixture {
    fixture_density     :: Dynamic t Double
  , fixture_friction    :: Dynamic t Double
  , fixture_restitution :: Dynamic t Double
  , fixture_shape       :: Dynamic t Shape
  , fixture_token       :: FixtureToken
  }

fixture :: SupportsReflexBox2D t m => Body t -> FixtureConfig t -> m (Fixture t)
fixture b (FixtureConfig d0 dd f0 df r0 dr s0 ds) = do
  f <- liftIO $ makeFixture (body_token b) (FixtureDef d0 f0 r0 s0)
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

data BodyConfig t = BodyConfig
  { bodyConfig_bodyType :: BodyType
  , bodyConfig_initialPosition :: (Vec2, Double)
  , bodyConfig_modifyPosition :: Event t ((Vec2, Double) -> (Vec2, Double))
  , bodyConfig_applyImpulse :: Event t ((Vec2,Double) -> (Vec2,Vec2))
  }

instance Reflex t => Default (BodyConfig t) where
  def = BodyConfig BodyTypeDynamic (Vec2 5 5, 0) never never


data Body t = Body {
    body_type      :: BodyType
  , body_position  :: Dynamic t (Vec2, Double)
  , body_token     :: BodyToken
  , body_contacts  :: Dynamic t () -- TODO
  }

trackChildren c0 dc f = listWithKeyShallowDiff c0 dc $ \k v dv ->
  widgetHold (f v) (f <$> dv)

-------------------------------------------------------------------------------
body'
  :: SupportsReflexBox2D t m
  => Event t ()
  -> WorldToken
  -> BodyConfig t
  -> (Body t -> m a)
  -> m (Body t, a)
body' sampleTicks w (BodyConfig bType (p0,a0) dp imp) children = do
  b <- liftIO $ createBody w (BodyDef bType p0 a0)
  -- liftIO $ bodySetType b bType >> bodySetX b x0 >> bodySetY b y0
  -- liftIO $ bodySetType b bType >> bodySetPositionAndAngle b p0 a0
  pForced <- performEvent $ ffor dp $ \f -> liftIO $ do
    v <- bodyGetPosition b
    rot <- return 0 -- TODO: get rotation
    let (p',r') = f (v,rot)
    bodySetPositionAndAngle b p' r'
    bodySetAwake b True
    return (p',r')

  pSimulation <- performEvent $ ffor sampleTicks $ \() -> liftIO $ do
    p <- bodyGetPosition b
    rot <- return 0 -- TODO: get rotation
    return (p, rot)

  p <- holdDyn (p0,a0) $ leftmost [pForced, pSimulation]

  performEvent $ ffor imp $ \f -> liftIO $ do
    p <- bodyGetPosition b
    r <- return 0 -- TODO: get angle
    let (impulse,center) = f (p,r)
    bodyApplyImpulse b impulse center
  let contacts = constDyn ()

  let bod = Body bType p b contacts
  a <- children bod
  return $ (bod, a)

body
  :: SupportsReflexBox2D t m
  => Event t ()
  -> WorldToken
  -> BodyConfig t
  -> (Body t -> m a)
  -> m a
body sampleTicks w
  bodyConf children = snd <$> body' sampleTicks w bodyConf children


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

