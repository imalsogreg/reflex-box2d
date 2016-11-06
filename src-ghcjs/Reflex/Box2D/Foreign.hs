{-# language OverloadedStrings #-}
{-# language ScopedTypeVariables #-}

module Reflex.Box2D.Foreign where

import Control.Applicative (liftA2)
import Control.Monad (mzero)
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import Data.Maybe (fromJust)
import GHCJS.Types
import GHCJS.DOM.Types (HTMLCanvasElement, unHTMLCanvasElement)
import GHCJS.Marshal
import qualified JavaScript.Object as JS
import qualified JavaScript.Cast as Cast
import Reflex.Box2D.Types


type WorldToken = JSVal
type FixtureDefToken = JSVal
type FixtureToken = JSVal
type BodyToken = JSVal
type BodyDefToken = JSVal

-------------------------------------------------------------------------------
-- COMMON
-------------------------------------------------------------------------------

instance ToJSVal Vec2 where
  toJSVal (Vec2 x y) = js_vec2 x y

instance FromJSVal Vec2 where
  fromJSVal val = do
    x <- js_vec2X val
    y <- js_vec2Y val
    return $ Just $ Vec2 x y

foreign import javascript "($1).x"
  js_vec2X :: JSVal -> IO Double

foreign import javascript "($1).y"
  js_vec2Y :: JSVal -> IO Double

foreign import javascript "console.log($1)"
  js_show :: JSVal -> IO ()

foreign import javascript unsafe "$r = new Box2D.Common.Math.b2Vec2($1,$2)"
  js_vec2 :: Double -> Double -> IO JSVal



-------------------------------------------------------------------------------
-- WORLD
-------------------------------------------------------------------------------

makeWorld :: Vec2 -> Bool -> IO WorldToken
makeWorld grav sleep = toJSVal grav >>= flip js_makeWorld sleep

worldSetGravity :: WorldToken -> Vec2 -> IO ()
worldSetGravity w v = toJSVal v >>= js_worldSetGravity w

worldSetAllowSleep :: WorldToken -> Bool -> IO ()
worldSetAllowSleep = js_worldSetAllowSleep

foreign import javascript unsafe "$r = new Box2D.Dynamics.b2World($1,$2)"
  js_makeWorld :: JSVal -> Bool -> IO JSVal

foreign import javascript unsafe "console.log($1)"
  js_showWorld :: JSVal -> IO ()

showWorld :: WorldToken -> IO ()
showWorld = js_showWorld

dirtyUpdate :: WorldToken -> IO ()
dirtyUpdate = js_dirtyUpdate

foreign import javascript unsafe "function l(x) {console.log(x);}; l('1'); var b2dd = Box2D.Dynamics.b2DebugDraw; l('2'); var dd = new b2dd(); l('3'); dd.SetSprite(document.getElementById(\"canvas\").getContext(\"2d\")); dd.SetDrawScale(30.0); dd.SetFillAlpha(0.3); dd.SetLineThickness(1.0); dd.SetFlags(b2dd.e_shapeBit | b2dd.e_jointBit); ($1).SetDebugDraw(dd);"
  js_dirtyDrawSetup :: WorldToken -> IO ()

foreign import javascript unsafe "($1).m_gravity=($2);"
  js_worldSetGravity :: JSVal -> JSVal -> IO ()
foreign import javascript unsafe "($1).m_allowSleep=($2);"
  js_worldSetAllowSleep :: JSVal -> Bool -> IO ()

drawSetup :: WorldToken -> HTMLCanvasElement -> IO ()
drawSetup w canv = js_drawSetup w (unHTMLCanvasElement canv)

worldStep :: WorldToken -> Double -> Int -> Int -> IO ()
worldStep = js_worldStep

foreign import javascript unsafe "function l(x) {console.log(x);}; l('1'); var b2dd = Box2D.Dynamics.b2DebugDraw; l('2'); var dd = new b2dd(); l('3'); dd.SetSprite(($2).getContext(\"2d\")); dd.SetDrawScale(30.0); dd.SetFillAlpha(0.3); dd.SetLineThickness(1.0); dd.SetFlags(b2dd.e_shapeBit | b2dd.e_jointBit); ($1).SetDebugDraw(dd);"
  js_drawSetup :: WorldToken -> JSVal -> IO ()

foreign import javascript unsafe "($1).Step($2,$3,$4);"
  js_worldStep :: JSVal -> Double -> Int -> Int -> IO ()

foreign import javascript unsafe "($1).Step(1/60,10,10); ($1).DrawDebugData(); ($1).ClearForces();"
  js_dirtyUpdate :: JSVal -> IO ()


foreign import javascript unsafe "($1).DrawDebugData();"
  js_drawDebugData :: JSVal -> IO ()

drawDebugData :: WorldToken -> IO ()
drawDebugData = js_drawDebugData



-------------------------------------------------------------------------------
-- FIXTURE
-------------------------------------------------------------------------------

instance ToJSVal FixtureDef where
  toJSVal (FixtureDef dens fric rest shpe) = do
    fd <- js_makeFixtureDef
    js_fixtureDefSetDensity fd dens
    js_fixtureDefSetFriction fd fric
    js_fixtureDefSetRestitution fd rest
    fixtureDefSetShape fd shpe
    return fd


createFixture :: BodyDefToken -> FixtureDefToken -> IO ()
createFixture = js_createFixture


makeFixture :: BodyToken -> FixtureDef -> IO FixtureToken
makeFixture b fd = toJSVal fd >>= \f -> js_makeFixture b f

fixtureDefSetDensity :: FixtureDefToken -> Double -> IO ()
fixtureDefSetDensity = js_fixtureDefSetDensity

fixtureDefSetFriction :: FixtureDefToken -> Double -> IO ()
fixtureDefSetFriction = js_fixtureDefSetFriction

fixtureDefSetRestitution :: FixtureDefToken -> Double -> IO ()
fixtureDefSetRestitution = js_fixtureDefSetRestitution

fixtureDefSetShape :: FixtureDefToken -> Shape -> IO ()
fixtureDefSetShape ft (ShapeBox w h) =  js_fixtureDefSetShapeRect ft w h
fixtureDefSetShape ft (ShapeCircle r) =  js_fixtureDefSetShapeCirc ft r

-- TODO: rename to createFixture
foreign import javascript unsafe "($1).CreateFixture($2);"
  js_createFixture :: JSVal -> JSVal -> IO ()


foreign import javascript unsafe "$r = new Box2D.Dynamics.b2FixtureDef()"
  js_makeFixtureDef :: IO JSVal

foreign import javascript unsafe "$r = ($1).CreateFixture($2)"
  js_makeFixture :: JSVal -> JSVal -> IO JSVal

foreign import javascript unsafe "($1).density=($2);"
  js_fixtureDefSetDensity :: JSVal -> Double -> IO ()
foreign import javascript unsafe "($1).friction=($2);"
  js_fixtureDefSetFriction :: JSVal -> Double -> IO ()
foreign import javascript unsafe "($1).restitution=($2);"
  js_fixtureDefSetRestitution :: JSVal -> Double -> IO ()
foreign import javascript unsafe "($1).shape = new Box2D.Collision.Shapes.b2PolygonShape(); ($1).shape.SetAsBox($2,$3);"
  js_fixtureDefSetShapeRect :: JSVal -> Double -> Double -> IO ()
foreign import javascript unsafe "($1).shape = new Box2D.Collision.Shapes.b2CircleShape($2);"
  js_fixtureDefSetShapeCirc :: JSVal -> Double -> IO ()

-------------------------------------------------------------------------------
-- BODY
-------------------------------------------------------------------------------

instance ToJSVal BodyDef where
  toJSVal (BodyDef typ pos rot) = do
    p  <- toJSVal pos
    bd <- js_bodyDef p rot
    bodyDefSetType bd typ
    return bd


createBody :: WorldToken -> BodyDef -> IO BodyToken
createBody w bd = toJSVal bd >>= js_createBody w

makeBodyDef :: IO BodyDefToken
makeBodyDef = js_makeBodyDef


bodyDefSetX :: BodyDefToken -> Double -> IO ()
bodyDefSetX = js_bodyDefSetX

bodyDefSetY :: BodyDefToken -> Double -> IO ()
bodyDefSetY = js_bodyDefSetY

bodyDefSetAng :: BodyDefToken -> Double -> IO ()
bodyDefSetAng = js_bodyDefSetAng

bodyDefGetX :: BodyDefToken -> IO Double
bodyDefGetX = js_bodyDefGetX

bodyDefGetY :: BodyDefToken -> IO Double
bodyDefGetY = js_bodyDefGetY

bodyDefSetType :: BodyDefToken -> BodyType -> IO ()
bodyDefSetType dt bt = js_bodyDefSetType dt (fromEnum bt)

foreign  import javascript unsafe "$r = new Box2D.Dynamics.b2BodyDef; $r.position = ($1); $r.angle = ($2);"
  js_bodyDef :: JSVal -> Double -> IO JSVal

foreign import javascript unsafe "$r = new Box2D.Dynamics.b2BodyDef()"
  js_makeBodyDef :: IO JSVal

foreign import javascript unsafe "$r = ($1).CreateBody($2)"
  js_createBody :: JSVal -> JSVal -> IO JSVal




foreign import javascript unsafe "($1).position.x"
  js_bodyDefGetX :: JSVal -> IO Double
foreign import javascript unsafe "($1).position.y"
  js_bodyDefGetY :: JSVal -> IO Double

foreign import javascript unsafe "($1).position.x=($2);"
  js_bodyDefSetX :: JSVal -> Double -> IO ()
foreign import javascript unsafe "($1).position.y=($2);"
  js_bodyDefSetY :: JSVal -> Double -> IO ()
foreign import javascript unsafe "($1).SetAngle($2);"
  js_bodyDefSetAng :: JSVal -> Double -> IO ()


bodyDefSetTransform :: BodyToken -> Vec2 -> Double -> IO ()
bodyDefSetTransform b p ang = toJSVal p >>= \jsp -> js_bodyDefSetTransform b jsp ang

foreign import javascript unsafe "console.log($1); ($1).position.Set(($2).x, ($2).y);"
  js_bodyDefSetTransform :: JSVal -> JSVal -> Double -> IO ()

foreign import javascript unsafe "($1).type = Box2D.Dynamics.b2Body[(($2)==0 ? 'b2_staticBody' : (($2)==1 ? 'b2_kinematicBody' : 'b2_dynamicBody'))];"
  js_bodyDefSetType :: JSVal -> Int -> IO ()



-- TODO: fixture{Get|Set}{Shape|Type|Density|Friction|Restitution|Sensor}

bodySetPositionAndAngle :: BodyToken -> Vec2 -> Double -> IO ()
bodySetPositionAndAngle b p a = toJSVal p >>= \v2 -> js_bodySetPositionAndAngle b v2 a

foreign import javascript unsafe "($1).SetPositionAndAngle($2,$3);"
  js_bodySetPositionAndAngle :: JSVal -> JSVal -> Double -> IO ()

bodyGetPosition :: BodyToken -> IO Vec2
bodyGetPosition t = do
  -- print "bodyGetPosition"
  -- js_show t
  jsPos <- js_bodyGetPosition t
  pos <- fromJSVal jsPos
  --  print pos
  case pos of
    Just x -> return x
    _      -> error $ "Bad pos decode"
  -- toJSVal t >>= js_bodyGetPosition >>= fmap fromJust . fromJSVal

foreign import javascript unsafe "$r = ($1).GetPosition();"
  js_bodyGetPosition :: JSVal -> IO JSVal

bodyGetAngle :: BodyToken -> IO Double
bodyGetAngle = js_bodyGetAngle

foreign import javascript unsafe "($1).GetAngle();"
  js_bodyGetAngle :: JSVal -> IO Double

bodySetAwake :: BodyToken -> Bool -> IO ()
bodySetAwake = js_bodySetAwake

foreign import javascript unsafe "($1).SetAwake($2);"
  js_bodySetAwake :: JSVal -> Bool -> IO ()

bodyGetAwake :: BodyToken -> IO Bool
bodyGetAwake = js_bodyGetAwake

foreign import javascript unsafe "($1).GetAwake()"
  js_bodyGetAwake :: JSVal -> IO (Bool)

bodyApplyImpulse :: BodyToken -> Vec2 -> Vec2 -> IO ()
bodyApplyImpulse b iDir iAt =
  uncurry (js_bodyApplyImpulse b) =<< liftA2 (,) (toJSVal iDir) (toJSVal iAt)

foreign import javascript unsafe "($1).ApplyImpulse($2,$3)"
  js_bodyApplyImpulse :: JSVal -> JSVal -> JSVal -> IO ()
