module Boids.Core.Update

open Boids.Core
open Boids.Core.Math
open Boids.Core.Model
open Boids.Core.Msg

module Boid =
  let move (speed : float32) (boid : Boid) : Boid =
    let v =
      {
        x = cos boid.angle
        y = sin boid.angle
      }
      * speed
    
    { boid with
        pos = boid.pos + v
    }

  let clamp config (boid : Boid) : Boid =
    let p = boid.pos
    let fs = config.field_position
    let fe = config.field_size + fs

    let calc_angle (xs, ys) =
      let v =
        {
          x = xs * cos boid.angle
          y = ys * sin boid.angle
        }

      let angle =
        Vec2.angle v
        |> Option.defaultValue boid.angle

      let x = p.x |> max fs.x |> min fe.x
      let y = p.y |> max fs.y |> min fe.y

      { boid with
          angle = angle
          pos = { x = x; y = y }
      }

    if p.x < fs.x || fe.x < p.x then
      calc_angle(-1.0f, 1.0f)
    elif p.y < fs.y || fe.y < p.y then
      calc_angle(1.0f, -1.0f)
    else
      boid

  let rotate angle (boid : Boid) : Boid =
    { boid with
        angle =
          mod' angle (2.0f * Angle.pi)
    }


module Model =
  let append boid (model : Model) =
    let boid = model.next_id, boid
  
    { model with
        boids = boid::model.boids
        next_id = model.next_id + 1
    }
  
  let move (model : Model) =
    { model with
        boids =
          model.boids
          |> List.map (fun (id, boid) ->
            id,
            boid
            |> Boid.move model.boid_params.speed
            |> Boid.clamp model.config
          )
    }

  let separate neighbor_pairs (model : Model) =
    let neighbor_pairs =
      neighbor_pairs
      |> Seq.filter(fun (_, distance) ->
        distance < model.boid_params.min_separate
      )

    let boids =
      model.boids
      |> Seq.map(fun (id, boid) ->
        let neighbors =
          Model.neighbors id neighbor_pairs
        
        let angle =
          neighbors
          |> Seq.sortBy snd
          |> Seq.toList
          |> function
          | [] -> boid.angle
          | (id_n, _)::_ ->
            maybe {
              let! boid_n = model |> Model.getBoid id_n
              let angle =
                mod' (boid_n.angle + Angle.pi) (2.0f * Angle.pi)
              let da = mod' (angle - boid.angle) (2.0f * Angle.pi)
              let da = if da > Angle.pi then da - (2.0f * Angle.pi) else da
              let da = (min (abs da) model.boid_params.max_separate_turn) * float32 (sign da)
              return mod' (da + boid.angle) (2.0f * Angle.pi)
                
            }
            |> Option.defaultValue boid.angle

        id, { boid with angle = angle }
      )
      |> Seq.toList
    
    { model with boids = boids }

  let align (neighbor_pairs) (model : Model) =
    let boids =
      model.boids
      |> Seq.map(fun (id, boid) ->
        let neighbors =
          Model.neighbors id neighbor_pairs

        let angles =
          neighbors
          |> Seq.filterMap (fun (id_n, _) ->
            maybe {
              let! boid_n =
                model |> Model.getBoid id_n
              let angle = boid_n.angle
              return
                { x = cos angle; y = sin angle }
            }
          )
          |> Seq.toList

        let angle =
          angles |> function
          | [] -> boid.angle
          | _ ->
            maybe {
              let! angle_cdn =
                ((List.fold (+) {x = 0.0f; y = 0.0f} angles) / float32(List.length angles))
              let! angle = Vec2.angle angle_cdn
              let da = mod' (angle - boid.angle) (2.0f * Angle.pi)
              let da = if da > Angle.pi then da - (2.0f * Angle.pi) else da
              let da = (min (abs da) model.boid_params.max_align_turn) * float32 (sign da)
              return mod' (da + boid.angle) (2.0f * Angle.pi)
            }
            |> Option.defaultValue boid.angle

        id, { boid with angle = angle }
      )
      |> Seq.toList

    { model with boids = boids}


  let cohesion (neighbor_pairs) (model : Model) =
    let boids =
      model.boids
      |> Seq.map(fun (id, boid) ->
        let neighbors =
          Model.neighbors id neighbor_pairs
          |> Seq.map fst
        
        let poss =
          neighbors
          |> Seq.filterMap(fun id -> Model.getBoid id model)
          |> Seq.map(fun x -> x.pos - boid.pos)
          |> Seq.toList
        
        let angle =
          poss |> function
          | [] -> boid.angle
          | _ ->
            maybe {
              let! diff_pos = 
                ((List.fold (+) {x = 0.0f; y = 0.0f} poss) / float32(List.length poss))
              let! angle = Vec2.angle diff_pos
              let da = mod' angle (2.0f * Angle.pi)
              let da = if da > Angle.pi then da - (2.0f * Angle.pi) else da
              let da = (min (abs da) model.boid_params.max_cohere_turn) * float32 (sign da)
              return mod' (da + boid.angle) (2.0f * Angle.pi)
            }
            |> Option.defaultValue boid.angle

        id, { boid with angle = angle }
      )
      |> Seq.toList

    { model with boids = boids}

  let out_of_field (model : Model) =
    let boids =
      model.boids
      |> List.filter(fun (_, boid) ->
        let p = boid.pos
        let fs = model.config.field_position
        let fe = model.config.field_size + fs

        (fs.x < p.x && p.x < fe.x) &&
        (fs.y < p.y && p.y < fe.y)
      )

    { model with boids = boids}

module Update =
  let update (model : Model) (msg : Msg.Msg) =
    msg |> function
    | Msg.Nothing ->
      let neighbor_pairs =
        model
        |> Model.neighbor_pairs
      
      let model =
        model
        |> Model.move
        |> Model.separate neighbor_pairs
        |> Model.align neighbor_pairs
        |> Model.cohesion neighbor_pairs
        // |> Model.out_of_field
      
      model, msg

    | Msg.MouseClick (position, angle) ->
      let model =
        model
        |> Model.append
          { pos = position
            angle = angle
          }
      
      model, Msg.Nothing


type ICoreHolder =
  abstract Model : Model.Model
  abstract Msg : Msg.Msg


type UpdateManager(config, boidparams) =
  let mutable model = Model.Model.init(config, boidparams)
  let mutable msg = Msg.Nothing
  
  interface ICoreHolder with
    member __.Model = model
    member __.Msg = msg

  member __.Update (imsg_list : IEnterMsg list) =
    let msg_list =
      imsg_list
      |> Seq.filterMap IEnterMsg.get_msg
      |> Seq.toList
    
    for x in imsg_list do
      x.Msg <- None
     
    let rec update model msg =
      function
      | h_msg::tail ->
        let model, msg =
          Update.update model h_msg
        update model msg tail
      | [] ->
        Update.update model msg
    

    let _model, _msg =
      update model msg msg_list
    
    model <- _model
    msg <- _msg
