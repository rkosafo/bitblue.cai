namespace BitBlue.Cai.Actors


module Subscriptions =
  open Hopac

  type ISubscribable = inherit System.IComparable


  let mutable private subscribers = Map.empty<ISubscribable, (unit->unit) list>


  module private SubscribeActor =
    open Hopac.Actors.ActorSystem

    type Message = 
      | Subscribe of ISubscribable * (unit -> unit)

    type Actor (name) =
      inherit GenericActor<Message, unit>
        ( name,
          fun ctx ->
            job {
              match ctx.message with
              | Subscribe (evt, handler) ->
                match Map.tryFind evt subscribers with
                | None -> subscribers <- Map.add evt [handler] subscribers
                | Some handlers -> subscribers <- Map.add evt (handler :: handlers) subscribers } )


  open SubscribeActor
  open Hopac.Actors
  open Hopac.Infixes

  let subscribe handler evt = 
    ActorSystem.actorOf<Actor> "0"
    >>= fun x -> x.tell' <| Subscribe (evt, handler)
  let subscribe' handler evt = subscribe handler evt |> start
  let publish evt =
    printfn "Publishing [%A]" evt
    match Map.tryFind evt subscribers with
    | None | Some [] -> ()
    | Some xs ->
      xs
      |> List.map (fun f -> job { do f () })
      |> Job.conIgnore
      |> Hopac.startWithActions
        (fun e -> printfn "Error publishing event [%A]: %A" evt e) ignore

  let registerActors () = job {
    ActorSystem.register Actor }