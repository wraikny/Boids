module Boids.Core.Model

open Boids.Core
open Boids.Core.Math

type Boid =
  { pos : float32 Vec2
    angle : float32 // Radian
  }


type BoidParams =
  {
    radius : float32
    /// 進行速度
    speed : float32
    /// これより近い距離にあるエージェントをneighborとする
    vision : float32
    /// この値よりも近いエージェントに対して分離を行う
    min_separate : float32
    /// 分離(Separation)が発生したときの変更される進行角度の最大絶対値
    max_separate_turn : float32
    /// 整列(Alignment)ルールで変更される進行角度の最大絶対値
    max_align_turn : float32
    /// 結合(Cohesion)ルールで変更される進行角度の最大絶対値
    max_cohere_turn : float32
  }

module BoidParams =
  let init() : BoidParams =
    {
      radius = 5.0f
      speed = 1.0f
      vision = 150.0f
      min_separate = 10.0f
      max_separate_turn = 2.0f |> Angle.deg_to_rad
      max_align_turn = 2.0f |> Angle.deg_to_rad
      max_cohere_turn = 4.0f |> Angle.deg_to_rad
    }

type GameConfig =
  {
    field_position : float32 Vec2
    field_size : float32 Vec2
  }


type ID = int32

type Model =
  { boids : (ID * Boid) list
    boid_params : BoidParams
    config : GameConfig
    next_id : ID
  }

module Model =
  let init(config, boidparams) =
    { boids = []
      boid_params = boidparams
      config = config
      next_id = 0 }

  let getBoid id model =
    model.boids
    |> Seq.tryAssoc id

  let neighbor_pairs model : seq<(ID * ID) * float32> =
    model.boids
    |> List.allPairs
    |> Seq.map(fun ((id1, b1), (id2, b2)) ->
      let distance =
        Vec2.length (b1.pos - b2.pos)
      ((id1, id2), distance)
    )
    |> Seq.filter(fun (_, distance) ->
      distance < model.boid_params.vision
    )

  let neighbors id pairs : seq<ID * float32> =
    pairs
    |> Seq.filterMap(fun ((id1, id2), distance) ->
      if (id1 = id2) then None
      elif id = id1 then Some (id2, distance)
      elif id = id2 then Some (id1, distance)
      else None
    )