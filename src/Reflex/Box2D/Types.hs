{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE DeriveGeneric #-}

module Reflex.Box2D.Types where

import GHC.Generics

data Vec2 = Vec2 !Double !Double
  deriving (Eq, Ord, Show, Generic)

data FixtureDef a = FixtureDef {
    fixtureDef_density :: Double
  , fixtureDef_friction :: Double
  , fixtureDef_restitution :: Double
  , fixtureDef_shape :: Shape
  , fixtureDef_token :: a
  }

data BodyDef a = BodyDef {
    bodyDef_type :: BodyType
  , bodyDef_positionX :: Double
  , bodyDef_positionY :: Double
  , bodyDef_token :: a
  }

data BodyType = BodyTypeStatic | BodyTypeDynamic
  deriving (Eq, Ord, Enum, Show, Generic)

data Shape = ShapeBox Double Double
           | ShapeCircle Double
           deriving (Eq, Ord, Show, Generic)
