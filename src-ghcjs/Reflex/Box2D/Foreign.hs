module Reflex.Box2D.Foreign where

import GHCJS.Types
import GHCJS.DOM.Types (HTMLCanvasElement, unHTMLCanvasElement)
import GHCJS.Marshal
import Reflex.Box2D.Types


type WorldToken = JSVal
type FixtureToken = JSVal
type BodyToken = JSVal
type BodyDefToken = JSVal

makeWorld :: Vec2 -> Bool -> IO WorldToken
makeWorld grav sleep = toJSVal grav >>= flip js_makeWorld sleep

worldCreateBodyAndFixture :: WorldToken -> BodyDefToken -> FixtureToken -> IO ()
worldCreateBodyAndFixture = js_worldCreateBodyAndFixture

worldSetGravity :: WorldToken -> Vec2 -> IO ()
worldSetGravity w v = toJSVal v >>= js_worldSetGravity w

worldSetAllowSleep :: WorldToken -> Bool -> IO ()
worldSetAllowSleep = js_worldSetAllowSleep


makeFixture :: IO FixtureToken
makeFixture = js_makeFixture

makeBody :: IO BodyToken
makeBody = js_makeBody

makeBodyDef :: IO BodyDefToken
makeBodyDef = js_makeBodyDef

fixtureDefSetDensity :: FixtureToken -> Double -> IO ()
fixtureDefSetDensity = js_fixtureDefSetDensity

fixtureDefSetFriction :: FixtureToken -> Double -> IO ()
fixtureDefSetFriction = js_fixtureDefSetFriction

fixtureDefSetRestitution :: FixtureToken -> Double -> IO ()
fixtureDefSetRestitution = js_fixtureDefSetRestitution

fixtureDefSetShape :: FixtureToken -> Shape -> IO ()
fixtureDefSetShape ft (ShapeBox w h) =  js_fixtureDefSetShapeRect ft w h
fixtureDefSetShape ft (ShapeCircle r) =  js_fixtureDefSetShapeCirc ft r

bodyDefSetX :: BodyDefToken -> Double -> IO ()
bodyDefSetX = js_bodyDefSetX

bodyDefSetY :: BodyDefToken -> Double -> IO ()
bodyDefSetY = js_bodyDefSetY

bodyDefSetType :: BodyDefToken -> BodyType -> IO ()
bodyDefSetType dt bt = js_bodyDefSetType dt (fromEnum bt)


instance ToJSVal Vec2 where
  toJSVal (Vec2 x y) = js_vec2 x y

foreign import javascript unsafe "$r = new Box2D.Dynamics.b2World($1,$2)"
  js_makeWorld :: JSVal -> Bool -> IO JSVal

foreign import javascript unsafe "console.log($1)"
  js_showWorld :: JSVal -> IO ()


foreign import javascript unsafe "($1).CreateBody($2).CreateFixture($3);"
  js_worldCreateBodyAndFixture :: JSVal -> JSVal -> JSVal -> IO ()

foreign import javascript unsafe "$r = new Box2D.Common.Math.b2Vec2($1,$2)"
  js_vec2 :: Double -> Double -> IO JSVal

foreign import javascript unsafe "$r = new Box2D.Dynamics.b2FixtureDef()"
  js_makeFixture :: IO JSVal

foreign import javascript unsafe "$r = new Box2D.Dynamics.b2BodyDef()"
  js_makeBodyDef :: IO JSVal

foreign import javascript unsafe "$r = new Box2D.Dynamics.b2Body()"
  js_makeBody :: IO JSVal

foreign import javascript unsafe "($1).m_gravity=($2);"
  js_worldSetGravity :: JSVal -> JSVal -> IO ()
foreign import javascript unsafe "($1).m_allowSleep=($2);"
  js_worldSetAllowSleep :: JSVal -> Bool -> IO ()



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

foreign import javascript unsafe "($1).position.x=($2);"
  js_bodyDefSetX :: JSVal -> Double -> IO ()
foreign import javascript unsafe "($1).position.y=($2);"
  js_bodyDefSetY :: JSVal -> Double -> IO ()
foreign import javascript unsafe "($1).type = Box2D.Dynamics.b2Body[(($2)==0 ? 'b2_staticBody' : 'b2_dynamicBody')];"
  js_bodyDefSetType :: JSVal -> Int -> IO ()

foreign import javascript unsafe "function l(x) {console.log(x);}; l('1'); var b2dd = Box2D.Dynamics.b2DebugDraw; l('2'); var dd = new b2dd(); l('3'); dd.SetSprite(document.getElementById(\"canvas\").getContext(\"2d\")); dd.SetDrawScale(30.0); dd.SetFillAlpha(0.3); dd.SetLineThickness(1.0); dd.SetFlags(b2dd.e_shapeBit | b2dd.e_jointBit); ($1).SetDebugDraw(dd);"
  js_dirtyDrawSetup :: WorldToken -> IO ()

drawSetup :: WorldToken -> HTMLCanvasElement -> IO ()
drawSetup w canv = js_drawSetup w (unHTMLCanvasElement canv)

foreign import javascript unsafe "function l(x) {console.log(x);}; l('1'); var b2dd = Box2D.Dynamics.b2DebugDraw; l('2'); var dd = new b2dd(); l('3'); dd.SetSprite(($2).getContext(\"2d\")); dd.SetDrawScale(30.0); dd.SetFillAlpha(0.3); dd.SetLineThickness(1.0); dd.SetFlags(b2dd.e_shapeBit | b2dd.e_jointBit); ($1).SetDebugDraw(dd);"
  js_drawSetup :: WorldToken -> JSVal -> IO ()

foreign import javascript unsafe "($1).Step($2,$3,$4);"
  js_worldStep :: JSVal -> Double -> Int -> Int -> IO ()

foreign import javascript unsafe "($1).Step(1/60,10,10); ($1).DrawDebugData(); ($1).ClearForces();"
  js_dirtyUpdate :: JSVal -> IO ()
