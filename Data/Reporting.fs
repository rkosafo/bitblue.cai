namespace BitBlue.Cai


module Reporting =
  open TheBeta
  open Newtonsoft.Json
  open Newtonsoft.Json.Linq
  //open System.IO
  //open System.Xml
  //open System.Text

  type ReportEntry =
    { id: int64
      name: string
      description: string
      path: string
      form: string option
      fields: Field list
      baseQuery: QueryExpr list
      baseFilter: FilterExpr list
      initQuery: string
      category: string
      fileUrl: string }


  type ReportEntryCategory =
    { id: string
      name: string
      description: string
      items: ReportEntry list }


  type ReportRequest =
    { path: string
      format: string
      download: bool
      q: QueryExpr list
      p: obj } with
    static member parseP (x: obj) =
      match x with
      | :? string as x -> JsonConvert.DeserializeObject<JObject> x
      | :? JObject as x -> x
      | _ ->
        printfn "Unknown type"
        JObject()
    static member getP x = ReportRequest.parseP x.p
