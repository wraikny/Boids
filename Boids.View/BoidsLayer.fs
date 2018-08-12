module Boids.View.BoidsLayer

open Boids
open Boids.View

type BoidsLayer(holder) =
  inherit asd.Layer2D()

  let holder : Core.Update.ICoreHolder = holder

  let mutable next_id : Core.Model.ID = 0

  let add_boids (layer : asd.Layer2D) =
    let next_id_current = holder.Model.next_id

    for id in next_id..(next_id_current - 1) do
      new Boid.Boid(id, holder)
      |> layer.AddObject

    next_id <- next_id_current
  
  override this.OnUpdated() =
    add_boids this