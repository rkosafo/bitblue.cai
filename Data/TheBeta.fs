namespace BitBlue.Cai


module TheBeta =
  open FSharp.Core
  open System
  open BitBlue.Utilities
  //open Cai.ServerBase 
  //open Cai.ServerBase.Extensions
  open BitBlue.Cai.Data.Utils

  let sanitize = sprintf "%O" //todo:
  let sanitizeField x = sprintf "[%s]" x

  
  type FieldType //todo: move out as general types for both models and reports
    = Text
    | Number
    | Date
    | DateTime
    | Bool
  module FieldType =
    let fromText (x:string) =
      match x.ToLower () with
      | "text" -> Text
      | "number" -> Number
      | "date" -> Date
      | "datetime" -> DateTime
      | "bool" -> Bool
      | x -> failwithf "Unknown field type: %s" x


  type Field =
    //= NamedField of name:string
    | NF of name:string * typ:FieldType with
    override x.ToString () =
      match x with
      //| NamedField x -> x
      | NF (x, _) -> x
  module Field =
    let name x =
      match x with
      | NF (n, _) -> n

    let without fieldNames fields =
      fields
      |> List.filter (fun x -> not <| List.contains (name x) fieldNames)


  type FilterExpr
    = Eq of field:string * value:obj
    | Gt of field:string * value:obj
    | Gte of field:string * value:obj
    | Lt of field:string * value:obj
    | Lte of field:string * value:obj
    | StartsWith of field:string * value:string
    | EndsWith of field:string * value:string
    | Contains of field:string * value:string
    | In of field:string * values:obj list
    | Not of field: string * value: obj
  module FilterExpr =
    let toString fields pIndex pos x = //todo: ensure fields
      let parameterName = sprintf "@p%i_%i" pIndex pos
      match x with
      | Eq (field, value) -> sprintf "(%s = %s)" (sanitizeField field) parameterName, [parameterName, sanitize value]
      | Gt (field, value) -> sprintf "(%s > %s)" (sanitizeField field) parameterName, [parameterName, sanitize value]
      | Gte (field, value) -> sprintf "(%s >= %s)" (sanitizeField field) parameterName, [parameterName, sanitize value]
      | Lt (field, value) -> sprintf "(%s < %s)" (sanitizeField field) parameterName, [parameterName, sanitize value]
      | Lte (field, value) -> sprintf "(%s <= %s)" (sanitizeField field) parameterName, [parameterName, sanitize value]
      | Not (field, value) -> sprintf "(%s <> %s)" (sanitizeField field) parameterName, [parameterName, sanitize value]
      | Contains (field, value) -> sprintf "(%s like %s)" (sanitizeField field) parameterName, [parameterName, "%" + sanitize value + "%"]
      | StartsWith (field, value) -> sprintf "(%s like %s)" (sanitizeField field) parameterName, [parameterName, sanitize value + "%"]
      | EndsWith (field, value) -> sprintf "(%s like %s)" (sanitizeField field) parameterName, [parameterName, "%" + sanitize value]
      | In (field, xs) ->
          match xs with
          | [] -> "0 = 0", []
          | xs ->
              let sqls, parameters =
                xs
                |> List.mapi (fun i x ->
                    let p = sprintf "%s__%i" parameterName i
                    p, (p, sanitize x))
                |> List.unzip
              let sql = sprintf "(%s in (%s))" (sanitizeField field) (String.Join(", ", sqls))
              sql, parameters
    ///Compares two filters and return the first one if they are equal
    let tryConcat x y =
      match x, y with
      | Eq (fx, vx), Eq (fy, vy) when fx = fy && vx=vy -> Some <| x
      | _ -> None
    let tryParse xs =
      let tryConvert value parserName = 
        try
          match parserName with
          | "int" -> int value |> box |> Some
          | "float" -> float value |> box |> Some
          | "decimal" -> float value |> box |> Some
          | _ -> 
              printfn "Invalid parserName: %s" parserName
              None
        with | e -> None
      match List.ofSeq xs with
      | "Eq" :: field :: [value] -> Some <| Eq (field, box value)
      | "Eq" :: field :: value :: parserName :: _ ->
          match tryConvert value parserName with
          | None -> None
          | Some value -> Some <| Eq (field, value)
      | xs ->
          printfn "Unsupported filter content: %A" xs
          None

  type SortExpr =
    | Asc of field:string
    | Desc of field:string with
    override x.ToString() =
      match x with
      | Asc x -> sprintf "[%s] ASC" x
      | Desc x -> sprintf "[%s] DESC" x

  type QueryExpr =
    | InitSelect of query:string
    | FilterBy of FilterExpr list
    | SortBy of SortExpr list
    | DropFields of string list

  module Query =
    open System.Data.SqlClient
    open System.Data
    
    ///Attempt to identify and report if a query is not just a select statement
    let isSafeSelect sql = false

    let fetchAsTable (connString:string) sql parameters =
      let parameters =
        parameters
        |> List.map (fun (k,v) -> SqlParameter(k, box v))
        |> Array.ofList
      use adapter = new SqlDataAdapter(sql, connString)
      adapter.SelectCommand.Parameters.AddRange (parameters)
      use dataset = new DataSet() 
      let _ = adapter.Fill(dataset)
      dataset.Tables.[0]

    let normalizeField (name:string) =
      let name = name.Split('(').[0].Trim()
      match name with
      | "int" | "bigint" | "decimal" | "numeric" -> Number
      | "bit" -> Bool
      | "datetime" | "datetime2" -> Date
      | "nvarchar" | "varchar"
      | _ -> Text
    ///Attempt to describe a query by returning the fields and their data types
    let getFields (connString:string) query =
      let sql = 
        """select [name], system_type_name as [type] 
        from sys.dm_exec_describe_first_result_set (@sql, null, 1)
        where is_hidden = 0"""
      let parameters = [ "@sql", query ]
      let data = 
        fetchAsTable connString sql parameters
        |> fun x -> x.Rows
        |> Seq.cast<DataRow>
        |> Seq.map (fun x -> string x.[0], normalizeField <| string x.[1])
        |> List.ofSeq
      data

    let rec private getParameters' undeclared (connString:string) query =
      let sql = "exec sp_describe_undeclared_parameters @tsql = @sql;"
      let parameters = [ "@sql", query ]
      try
        let data =
          fetchAsTable connString sql parameters
          |> fun x -> x.Rows
          |> Seq.cast<DataRow>
          |> Seq.map (fun x -> string x.["name"], normalizeField <| string x.["suggested_system_type_name"])
          |> List.ofSeq
        undeclared, data, None
      with 
        | e ->
          if e.Message.StartsWith("The undeclared parameter") then
            printfn "%A" e.Message
            let tokens = e.Message.Split '\'' |> List.ofArray
            match tokens with
            | _ :: x :: _ ->
              let query = sprintf "Declare %s varchar; %s" x query
              getParameters' (x :: undeclared) connString query
            | _ ->
              printfn "Interesting but the message [%s] is not what I expected" e.Message
              [], [], Some "I could not parsing. Please look at the query again"
          else 
            printfn "%A" e
            [], [], Some (parseException e)
      //todo: this will fail if the parameter is used more than once.
  
    let getParameters (connString:string) query = 
      match getParameters' [] connString query with
      | _, _, Some x -> Error x
      | xs, ys, _ ->
        let xs = xs |> List.map (fun x -> x.[1..])
        let ys = ys |> List.map (fun (x, y) -> x.[1..], y)
        Ok (xs, ys)

    let describe (connString:string) query =
      match getParameters connString query with
      | Error x -> Error x
      | Ok (undeclared, declared) ->
        let fields = 
          match undeclared, declared with
          | [], [] -> getFields connString query
          | xs, ys ->
            let query =
              (xs |> List.map (fun x -> x, Text))
              @ ys
              |> List.fold (fun (state:string) (name, typ) ->
                let value =
                  match typ with
                  | Number | Bool -> "0"
                  | Date | DateTime -> "'2017-01-01'"
                  | _ -> "''"
                System.String.replace ("@"+name) value false state) query
            getFields connString query
        Ok (undeclared, declared, fields)
    
    let fetch (connString:string) sql parameters preview =
      let sql =
        if preview then sprintf "SELECT TOP(5) * FROM (%s) as y1" sql //todo: change to fetch
        else sql
      let parameters =
        parameters
        |> List.map (fun (k,v) -> SqlParameter(k, box v))
        |> Array.ofList
      use adapter = new SqlDataAdapter(sql, connString)
      adapter.SelectCommand.Parameters.AddRange (parameters)
      use dataset = new DataSet() 
      let _ = adapter.Fill(dataset)
      let table = dataset.Tables.[0]
      let columns = table.Columns |> Seq.cast<DataColumn> |> Seq.map (fun x -> x.ColumnName) |> List.ofSeq
      let data =
        table.Rows
        |> Seq.cast<DataRow>
        |> Seq.map (fun x -> x.ItemArray)
        |> List.ofSeq
      columns, data

  let parse fields query =
    let initSelect activeFields query =
      activeFields, [], query
    let select name activeFields fields =
      let fields =
        match activeFields, fields with
        | [], _ -> failwith "Entity has no fields. Operation aborted"
        | xs, [] -> xs
        | xs, ys ->
            let names = xs |> List.map (fun f -> Field.name(f).ToLower()) 
              //todo: get the correct field from the main once. Fail on non existing fields
              //todo: handle computed fields
            ys
            |> List.filter (fun f -> List.contains (Field.name(f).ToLower()) names)
      match fields with
      | [] -> failwith "No fields are avaialble for selection. Operation aborted" // sprintf "select * from %s" name
      | xs ->
          let names = xs |> List.map (string >> sanitizeField)
          let allFields = String.Join(", ", names)
          fields, [], sprintf "SELECT %s from %s" allFields name
    let filterBy index fields sql ops =
      match ops with
      | [] -> fields, [], sql
      | xs ->
          //todo: use the field type to determine the value to pass
          let ops, parameters = xs |> List.mapi (fun i x -> FilterExpr.toString fields index i x) |> List.unzip
          let allOps = String.Join(" AND ", ops)
          fields, parameters, sprintf "SELECT * FROM (%s) as x%i WHERE %s" sql index allOps
    let dropFields index fields sql fieldNames =
      match fieldNames with
      | [] -> fields, [], sql
      | xs ->
          let fields =
            fields
            |> List.map (fun f ->
                if fieldNames |> List.exists (fun n -> String.Equals(n, Field.name f, StringComparison.OrdinalIgnoreCase))
                then None
                else Some f)
            |> List.choose id
          let names = fields |> List.map (Field.name >> sanitizeField)
          let allFields = String.Join(", ", names)
          let sql = sprintf "SELECT %s from (%s) as x%i" allFields sql index
          fields, [], sql
    let sortBy index fields sql ops =
      match ops with
      | [] -> fields, [], sql
      | xs ->
          let ops = xs |> List.map string //todo: ensure all fields are present
          let allOps = String.Join(", ", ops)
          fields, [], sprintf "SELECT TOP 100 PERCENT * FROM (%s) AS x%i ORDER BY %s" sql index allOps
    let rec parse index activeFields sql expr = //todo: check for sql injection
      match expr with
      | InitSelect query -> initSelect activeFields query
      //| Select fields -> select entityName activeFields fields
      | FilterBy ops -> filterBy index activeFields sql ops
      | SortBy ops -> sortBy index activeFields sql ops
      | DropFields fs -> dropFields index activeFields sql fs
    query
    |> List.mapi (fun i x -> i, x)
    |> List.fold (fun (fields, (parameters, sql)) (i, expr) -> 
        let fs, ps, sql = parse i fields sql expr
        let ps = List.collect id ps
        fs, (ps @ parameters, sql) ) (fields, ([], ""))
    |> snd