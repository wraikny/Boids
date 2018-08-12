[<AutoOpen>]
module Boids.Core.Alias

module Seq =
  let filterMap f =
    Seq.map f
    >> Seq.filter Option.isSome
    >> Seq.map Option.get

  let tryAssoc key =
    Seq.tryFind(fst >> (=) key)
    >> Option.map(snd)


module List =
  let allPairs list =

    let rec calc l1 l2 result =
      match l1, l2 with
      | h1::t1, h2::t2 ->
        let result =
          (List.map (fun x -> (h1, x)) l2)
          @ result
        calc t1 t2 result
      | _, _ ->
        result
    
    match list with
    | [] -> []
    | h::t ->
      calc list t []


let inline (!!) (a : ^a) (b : ^b) = a, b

let inline mod' a b =
  ((a % b) + b) % b
