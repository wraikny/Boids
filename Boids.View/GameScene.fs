module Boids.View.GameScene

open Boids
open Boids.Core.Alias
open Boids.View
open Boids.View.BoidsLayer
open Boids.View.MouseMsgComponent
open Boids.View.BackLayer


type GameScene() =
  inherit asd.Scene()

  let config : Core.Model.GameConfig =
    let margin = 0.05f
    let window_size =
      asd.Engine.WindowSize.To2DF()
      |> Math.Vec2.from_asd_F
    {
      field_position = window_size * margin
      field_size = window_size * (1.0f - margin * 2.0f)
    }

  let boidparams : Core.Model.BoidParams =
    {
      radius = 5.0f
      speed = 2.0f
      vision = 150.0f
      min_separate = 50.0f
      max_separate_turn =
        3.0f
        |> Core.Math.Angle.deg_to_rad
      max_align_turn =
        20.0f
        |> Core.Math.Angle.deg_to_rad
      max_cohere_turn =
        5.0f
        |> Core.Math.Angle.deg_to_rad
    }
  
  let manager =
    Core.Update.UpdateManager(config, boidparams)

  let back_layer = new BackLayer(config)
  let boids_layer = new BoidsLayer(manager)

  let mouse_msg = new MouseMsgComponent()

  let ienter_msgs : Core.Msg.IEnterMsg list =
    [
      mouse_msg
    ]
  
  override this.OnRegistered() =
    this.AddLayer back_layer
    this.AddLayer boids_layer

    (mouse_msg, "MouseMsg")
    |> this.AddComponent
  
  
  override __.OnUpdated() =
    manager.Update ienter_msgs

    #if DEBUG
    if asd.Engine.Tool.Begin "Boids" then
      asd.Engine.Tool.Text <|
        sprintf "Model: %A" (manager :> Core.Update.ICoreHolder).Model
      asd.Engine.Tool.Text <|
        sprintf "Msg: %A" (manager :> Core.Update.ICoreHolder).Msg
      asd.Engine.Tool.End()
    #endif