namespace BitBlue.Cai.CacheStores


open Hopac
open BitBlue.Cai.Utils


type CacheStore<'p, 'd> (name, loader: ('p -> Job<'d list>), parameters:'p, refreshHandler) =
  let mutable store = []
  let refresh' () =
    job { 
      let! data = loader parameters
      store <- List.ofSeq data }
  let doRefresh () =
    refresh' ()
    |> startWithActions (fun e -> printfn "Error loading cache [%s, %A]: %A" name parameters e) ignore
  let tryFind' f = List.tryFind f store
  do 
    let handler = throttle 500 doRefresh
    refreshHandler handler

  member __.tryFind f = tryFind' f
  member __.refresh () = refresh' ()
  member __.filter f = List.filter f store
  member __.getAll () = store


type MapCacheStore<'p, 'd, 'k when 'k: comparison> (name, loader: ('p -> Job<'d list>), mapper: 'd -> 'k, parameters:'p, refreshHandler) =
  let mutable store = Map.empty
  let refresh' () =
    job { 
      let! data = 
        loader parameters
        |> Job.map (List.map (fun x -> mapper x, x))
      store <- 
        data 
        |> List.distinctBy fst
        |> Map.ofList }
  let doRefresh () =
    refresh' ()
    |> startWithActions (fun e -> printfn "Error loading cache [%s, %A]: %A" name parameters e) ignore
  do 
    let handler = throttle 500 doRefresh
    refreshHandler handler

  member __.refresh () = refresh' ()
  member __.tryFind k = Map.tryFind k store
  member __.tryPick f = Map.tryPick f store
  member __.getAll () = store
