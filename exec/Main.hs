{-# language FlexibleContexts  #-}
{-# language OverloadedStrings #-}
{-# language RecursiveDo       #-}
{-# LANGUAGE TypeFamilies      #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Monoid            ((<>))
import qualified Data.Text as T
import Data.Traversable
import Data.Time
import GHCJS.DOM.Types        (castToHTMLCanvasElement)
import Reflex.Dom
import Reflex.Box2D
import Reflex.Box2D.Types
import System.Random

-- temporary for test:
import Reflex.Box2D.Foreign

main = m'

m' :: IO ()
m' = mainWidget run

------------------------------------------------------------------------------
run :: SupportsReflexBox2D t m => m ()
run = do
  t0 <- liftIO getCurrentTime
  pb <- getPostBuild
  running <- toggle False =<< button "Running"
  printW  <- button "Printing World"
  lowerFloor <- button "LowerFloor"
  floorY <- foldDyn (\e acc -> acc - e) 10 ((-1) <$ lowerFloor)
  moveBall <- foldDyn (\e acc -> acc + e) 0 . (1 <$) =<< button "MoveBall"
  bumps <- tickLossy 2 t0
  (canv, _) <- elAttr' "canvas"
    ("id"     =: "canvas" <> "width"  =: "500"    <> "height" =: "500") blank
  let canvEl = castToHTMLCanvasElement $ _element_raw canv
      floorFConf   = def { fixtureConfig_initialShape = ShapeBox 20 1 }
      floorBConf = def { bodyConfig_initialFixtures = "floor" =: floorFConf
                       , bodyConfig_initialPosition = (Vec2 5 10, 0)
                       , bodyConfig_modifyPosition =
                           ffor (updated floorY) $ \y' -> \_ -> (Vec2 10 y',0)
                       , bodyConfig_bodyType = BodyTypeStatic
                       }
      ballFConf = def { fixtureConfig_initialShape = ShapeCircle 0.6 }
      ballBConf =  def { bodyConfig_initialFixtures = "floor" =: ballFConf
                       , bodyConfig_initialPosition = (Vec2 10 3, 0)
                       , bodyConfig_modifyPosition =
                           ffor (updated moveBall) $ (\x' -> \(Vec2 x y, r) -> (Vec2 x' (y - 0.5), r))
                       , bodyConfig_bodyType = BodyTypeDynamic
                       , bodyConfig_applyImpulse = (\(Vec2 x y,_) ->
                         (Vec2 (0.1) (-10), Vec2 x y)) <$ bumps
                       }
  w <- world (Just canvEl)
       def { worldConfig_initialBodies = "f" =: floorBConf <> "b" =: ballBConf
           , worldConfig_physicsRunning = current running
           , worldConfig_drawRunning    = current running
           }

  liftIO (drawSetup (world_token w ) canvEl)
  performEvent $ liftIO (showWorld $ world_token w) <$ printW
  -- dynText $ traceDyn "ballpos" $ (T.pack . show) <$> bodyDef_position b
  return ()


-- m :: IO ()
-- m = setup >>= \w -> (mainWidget $ do
--                            printBtn <- button "Print"
--                            performEvent $ (liftIO $ showWorld w) <$ printBtn
--                            run' w
--                        )


-- setup :: IO WorldToken
-- setup = do
--   w <- makeWorld (Vec2 0 10) True
--   floorFix <- makeFixture
--   fixtureDefSetDensity floorFix 1
--   fixtureDefSetFriction floorFix 0.5
--   fixtureDefSetRestitution floorFix 0.2
--   b <- makeBodyDef
--   bodyDefSetType b BodyTypeStatic
--   bodyDefSetX b 9
--   bodyDefSetY b 13
--   fixtureDefSetShape floorFix (ShapeBox 10 0.5)
--   worldCreateBodyAndFixture w b floorFix
--   return w

-- addSomething :: WorldToken -> IO BodyDefToken
-- addSomething w = do
--   someFix <- makeFixture
--   bod <- makeBodyDef
--   [x,y,i,a,b] <- forM [0..4] $ \_ -> randomRIO (0,1)
--   let shape | i < 0.5   = ShapeBox    (a + 0.1) (b + 0.1)
--             | otherwise = ShapeCircle (a + 0.1)
--   fixtureDefSetShape someFix shape
--   fixtureDefSetDensity someFix 1
--   fixtureDefSetDensity someFix 0.5
--   fixtureDefSetDensity someFix 0.2
--   bodyDefSetType bod BodyTypeDynamic
--   bodyDefSetX bod (x*10)
--   bodyDefSetY bod (y*10)
--   worldCreateBodyAndFixture w bod someFix
--   return bod

-- recenterSomething :: BodyDefToken -> IO ()
-- recenterSomething t = do
--   print "Hi"
--   bodyDefSetX t 0
--   bodyDefSetY t 0

-- run' :: MonadWidget t m => WorldToken -> m ()
-- run' w = do
--   b <- button "Grav"
--   d <- button "Recenter"
--   canv <- fst <$> elAttr' "canvas" ("id" =: "canvas"
--                                    <> "width" =: "500"
--                                    <> "height" =:  "600") blank
--   pb <- getPostBuild
--   t0 <- liftIO getCurrentTime
--   ts <- tickLossy (1/60) t0
--   ps  <- liftIO newStdGen >>= \g -> poissonLossy g 0.1 t0
--   performEvent_ $ (liftIO (randomRIO (-10,10) >>= \g -> worldSetGravity w (Vec2 0 g))) <$ b
--   -- performEvent_ (liftIO (js_dirtyDrawSetup w) <$ pb)
--   performEvent_ (liftIO (drawSetup w (castToHTMLCanvasElement $ _element_raw canv)) <$ pb)
--   performEvent_ (liftIO (dirtyUpdate w) <$ ts)
--   performEvent_ (liftIO (addSomething w >> return ()) <$ ps)
--   c <- liftIO $ addSomething w
--   cs <- performEvent_ (liftIO (recenterSomething c) <$ d)
--   e <- fmap fst $ el' "div" $ text "Hello"
--   performEvent_ (liftIO (showWorld w) <$ domEvent Click e)
--   display . fmap (id :: Int -> Int) =<< count (domEvent Click e)
