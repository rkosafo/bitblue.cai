namespace BitBlue.Cai.Explorer


open Hopac
open System.Text.RegularExpressions
open Service


type ExplorerRequest =
  { path: string
    op: FileOperation
    file: FileInfo option
    dir: DirInfo option
    ip: string }


type ExplorerResponse =
  { content: obj }


type ExplorerContext =
  { request: ExplorerRequest
    response: ExplorerResponse
    user: obj option }


type ExplorerPart = ExplorerContext -> Job<ExplorerContext option>
module ExplorerPart =
  let fail<'a> = Job.result Option<'a>.None

  let inline succeed x = Job.result <| Some x
  
  let bind (f: 'a -> Job<'b option>) (a: Job<'a option>) =
    job {
      let! p = a
      match p with
      | None -> return None
      | Some q ->
        let r = f q
        return! r }

  let compose first second x = bind second (first x)

  let rec choose (options: ExplorerPart list) ctx =
    job {
      match options with
      | [] -> return None
      | x :: xs ->
        let! res = x ctx
        match res with
        | Some x -> return Some x
        | None -> return! choose xs ctx }


module Successful =
  open ExplorerPart

  let fileResponse (file: FileInfo) ctx =
    let response = { ctx.response with content = file }
    { ctx with response = response } |> succeed

  let dirResponse (xs: ExplorerItem list) ctx =
    let response = { ctx.response with content = xs }
    { ctx with response = response } |> succeed


module Filters =
  open Hopac

  module private Option =
    let iif b x =
      if b then Some x else None

  let path s (x: ExplorerContext) =
    Job.result <| Option.iif (s = x.request.path) x

  let pathStarts s (x: ExplorerContext) =
    Job.result <| Option.iif (x.request.path.StartsWith s) x

  let url x = path x

  let pathRegex regex (x: ExplorerContext) =
    Job.result <| Option.iif (Regex.IsMatch(x.request.path, regex)) x

  let urlRegex x = pathRegex x


  open Sscanf
  open ExplorerPart

  let pathScan (pf: PrintfFormat<_, _, _, _, 't>) (h: 't -> ExplorerPart): ExplorerPart =
    let scan url =
      try
        let r = sscanf pf url
        Some r
      with _ -> None
    let F (r: ExplorerContext) =
      match scan r.request.path with
      | Some x ->
        let part = h x
        part r
      | None -> fail
    F

  let urlScan s x = pathScan s x


module Operators =
  let (>>=) a b = ExplorerPart.bind b a

  let (>=>) a b = ExplorerPart.compose a b


[<AutoOpen>]
module Functions =
  let handle (part: ExplorerPart) ctx =
    job {
      let! resp = part ctx
      match resp with
      | None -> return None
      | Some ctx -> return Some ctx.response }