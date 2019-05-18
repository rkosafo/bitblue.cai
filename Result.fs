namespace BitBlue.Cai


type IBoxedResult =
  abstract boxes : CaiResult<obj, obj>

and CaiResult<'ok, 'err> =
    | Error  of 'err 
    | Ok of 'ok
    interface IBoxedResult with
      member x.boxes =
        match x with
        | Ok v -> Ok (box v)
        | Error v -> Error (box v)


module CaiResult =
  let ok x = Ok x

  let orDefault x = function Ok x -> x | _ -> x

  let defaultValue x = function Ok x -> x | _ -> x

  let map f = function
    | Error e -> Error e
    | Ok x -> Ok (f x)

  let ofFunc f = try Ok (f ()) with | e -> Error e

  let ofFunc1 a1 f = try Ok (f a1) with | e -> Error e