module Boids.View.BackLayer

open Boids
open Boids.View
open Boids.View.Math

type BackLayer(config : Core.Model.GameConfig) =
  inherit asd.Layer2D()

  let back_frame =
    new asd.GeometryObject2D(
      Color = Params.Window.back_color,
      Shape =
        new asd.RectangleShape(
          DrawingArea =
            new asd.RectF(
              config.field_position |> Vec2.into_asd_F,
              config.field_size |> Vec2.into_asd_F
            )
        )
    )

  override this.OnAdded() =
    this.AddObject back_frame