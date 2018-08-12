module Boids.View.Math

module Vec2 =
  open Boids.Core.Math

  let into_asd_I (v : int Vec2) =
    new asd.Vector2DI(v.x, v.y)
  let into_asd_F (v : float32 Vec2) =
    new asd.Vector2DF(v.x, v.y)
  let from_asd_I (v : asd.Vector2DI) =
    {
      x = v.X
      y = v.Y
    }
  let from_asd_F (v : asd.Vector2DF) =
    {
      x = v.X
      y = v.Y
    }