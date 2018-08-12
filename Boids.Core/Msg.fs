module Boids.Core.Msg

open Boids.Core.Math

type Msg
  = Nothing
  | MouseClick of float32 Vec2 * float32

type IEnterMsg =
  abstract Msg : Msg option with get, set

module IEnterMsg =
  let get_msg (x : IEnterMsg) =
    x.Msg