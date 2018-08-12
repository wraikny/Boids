module Program
open Boids.View

[<EntryPoint>]
let main _ =
  asd.Engine.Initialize("Boids", 800, 600, new asd.EngineOption())
  |> ignore

  #if DEBUG
  asd.Engine.OpenTool()
  #endif

  new GameScene.GameScene()
  |> asd.Engine.ChangeScene

  while asd.Engine.DoEvents() do
    asd.Engine.Update()

  
  #if DEBUG
  asd.Engine.CloseTool()
  #endif
  
  asd.Engine.Terminate()

  0