module Boids.View.MouseMsgComponent

open Boids
open Boids.View
open Boids.View.Math

type MouseMsgComponent() =
  inherit asd.SceneComponent()

  let rand = new System.Random()

  interface Core.Msg.IEnterMsg with
    member val Msg = None with get, set

  override this.OnUpdated() =
    let mouse = asd.Engine.Mouse
    if mouse.LeftButton.ButtonState = asd.MouseButtonState.Release then
      (this :> Core.Msg.IEnterMsg).Msg <- 
        mouse.Position
        |> Vec2.from_asd_F
        |> (fun p -> Core.Msg.Msg.MouseClick(p, float32(rand.NextDouble()) * 2.0f * Core.Math.Angle.pi ))
        |> Some

  