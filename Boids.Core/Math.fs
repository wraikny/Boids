module Boids.Core.Math

module Angle =
  let pi = 3.14159265f
  let rad_to_deg r =
    r / pi * 180.0f
  
  let deg_to_rad d =
    d / 180.0f * pi

type ^a Vec2 =
  { x : ^a; y : ^a }
  static member inline (~-) a =
    { x = -a.x; y = -a.y }
  static member inline (+) (a, b) =
    { x = a.x + b.x; y = a.y + b.y }
  static member inline (-) (a, b) =
    { x = a.x - b.x; y = a.y - b.y }
  static member inline (*) (a, b) =
    { x = a.x * b; y = a.y * b }
  static member inline (*) (a, b) =
    { x = a * b.x; y = a * b.y }
  static member inline (/) (a, b) =
    try
      Some { x = a.x / b; y = a.y / b }
    with
    | e -> None

module Vec2 =
  let inline squared_length(v : ^a Vec2) : ^a =
    v.x ** 2.0f + v.y ** 2.0f
  let inline length (v : ^a Vec2) : ^a =
    (squared_length v) ** 0.5f
  let inline normalize(v : ^a Vec2) : ^a Vec2 option =
    let q = (length v)
    try
      v * (q ** -1.0f)
      |> Some
    with
    | _ ->
      None

  let inline angle (v : float32 Vec2) =
    if v.x <> 0.0f then
      let angle =
        atan (v.y / v.x)
        + 
        if (float32 v.x > 0.0f) then 0.0f
        else Angle.pi

      Some angle
    elif v.y > 0.0f then
      Some (Angle.pi * 0.5f)
    elif v.y < 0.0f then
      Some (Angle.pi * 1.5f)
    else
      None