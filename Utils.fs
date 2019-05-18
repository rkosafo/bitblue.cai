namespace BitBlue.Cai


module Utils =
  open System
  open System.Timers
  open Newtonsoft.Json

  ///Expands the given template once
  let expand template (data:Map<string, string>) =
    let pattern = "\\${(.*?)}"
    let f (m:Regex.RegexMatch) =
      let key = m.Groups.[1].Value
      match Map.tryFind key data with
      | Some x -> x
      | _ -> m.Groups.[0].Value
    Regex.replaceWithFunction pattern f template

  ///Expands the template multiple times. This is to handle instances where the data values are templates
  let nExpand n template data =
    let rec loop (template:string) data n =
      if n=0 || not (template.Contains "${") then template
      else loop (expand template data) data (n-1)
    loop template data n

  let once waitTime callback =
    match waitTime with
    | 0. -> callback ()
    | _ ->
    let t = new Timer(waitTime * 1000.)
    t.Elapsed.Add(fun _ -> callback(); t.Close())
    t.Start()

  let toJson x = JsonConvert.SerializeObject (x, new Fable.JsonConverter())

  let ofJson<'t> x = JsonConvert.DeserializeObject<'t>(x, new Fable.JsonConverter())

  let ofJson'<'t> x = (string >> ofJson<'t>) x

  let tryParse parser x =
    try parser x |> Core.Ok
    with | e -> Core.Error e.Message

  let tryParse' parser ifNoneMsg x =
    match x with
    | None -> Core.Error ifNoneMsg
    | Some x -> tryParse parser x

  open System.Threading

  let throttle =
    let createTimer interval (state: int) callback =
      let ticker = new Timer (TimerCallback(callback), state, interval, System.Threading.Timeout.Infinite)
      ticker
    fun intervalMsg f ->
      let mutable result = None
      let mutable cnt = 0
      let mutable timer = None
      let rec handler () =
        match result with
        | None ->
          result <- Some <| f ()
          timer <- Some <|
            createTimer intervalMsg cnt
              (fun pCnt ->
                 match timer with | Some timer -> timer.Dispose () | _ -> ()
                 result <- None
                 if pCnt = unbox cnt
                 then cnt <- 0
                 else 
                   cnt <- 0
                   handler () |> ignore )
          result.Value
        | Some x ->
          cnt <- cnt + 1
          x        
      handler


