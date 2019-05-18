namespace BitBlue.Cai.Data


module Utils =
  open System.Data.SqlClient

  let parseException (e:exn) =
    let rec getLast (e:exn) =
      if isNull e.InnerException then e
      else getLast e.InnerException
    let e = getLast e
    match e with
    | :? SqlException as e ->
      if e.Message.Contains "conflicted with the REFERENCE constraint" then "This is referenced by other record so I cannot proceed"
      elif e.Message.Contains "conflicted with the FOREIGN KEY constraint" then "I could not find the association. Please refresh and try again"
      elif e.Message.Contains "Cannot insert duplicate key in object" then "I found a similar entry so this request has been rejected"
      else e.Message
    | _ -> e.Message