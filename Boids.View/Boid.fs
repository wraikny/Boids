module Boids.View.Boid

open Boids
open Boids.Core.Alias
open Boids.Core.Math
open Boids.View
open Boids.View.Math

module private Boid =
  let shape (model : Core.Model.Model) =
    let t = new asd.TriangleShape()
    for index in -1..1 do
      let p =
        new asd.Vector2DF(
          model.boid_params.radius * 2.0f, 0.0f,
          Degree = Params.Boid.degree * float32 index
        )
  
      t.SetPointByIndex(p, mod' index 3)
    t

type Boid(id : Core.Model.ID, holder : Core.Update.ICoreHolder) =
  inherit asd.GeometryObject2D(
    Shape = Boid.shape holder.Model,
    Color = Params.Boid.color
  )

  let id : int = id
  let holder = holder

  let transform (this : Boid) =
    holder.Model
    |> Core.Model.Model.getBoid id
    |> function
    | Some(boid) ->
      this.Position <- Vec2.into_asd_F boid.pos
      this.Angle <- boid.angle |> Angle.rad_to_deg
    
    | None ->
      this.Dispose()
  
  override this.OnAdded() =
    transform this
  override this.OnUpdate() =
    transform this