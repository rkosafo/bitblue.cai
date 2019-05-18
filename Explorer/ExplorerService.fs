namespace BitBlue.Cai.Explorer


module rec Service =
  open System
  open BitBlue.Cai
  //open Cai.ServerBase.Utils
  open BitBlue.Cai.TheBeta


  [<Flags>]
  type FileAction =
    | None = 0
    | Create = 1
    | Read = 2
    | Save = 4
    | Delete = 8
  module FileAction =
    let getOp entity src =
      match src |> Map.tryFind entity with
      | None -> FileAction.None
      | Some (x: Permission) ->
        if x.read then FileAction.Read else FileAction.None
        ||| if x.create then FileAction.Create else FileAction.None
        ||| if x.update then FileAction.Save else FileAction.None
        ||| if x.delete then FileAction.Delete else FileAction.None

  type OpenMode = Within | Without | Popup

  type FileOperation = CreateFile | SaveFile | DeleteFile | ReadFile

  type ExplorerAction =
    { icon: string
      item: ExplorerItem
      tooltip: string option
      openMode: OpenMode }

  type FileInfo =
    { name: string
      ext: string
      ops: FileAction
      path: string
      actions: ExplorerAction list
      description: string option
      icon: string option
      meta: obj option
      content: obj option }
  module FileInfo =
    let empty =
      { name = ""
        ext = ""
        ops = FileAction.None
        path = ""
        actions = []
        meta = None
        description = None
        icon = None
        content = None }
    let tryGetMeta parser file =
      match file.meta with
      | None -> Error "No meta found"
      | Some x -> 
        match Utils.tryParse parser x with
        | Core.Ok x -> Ok x
        | Core.Error x -> Error x


  type DirProps =
    { actions: ExplorerAction list
      query: FilterExpr list
      canRefresh: bool
      canSearch: bool
      description: string option
      fetchFileContent: bool }
  module DirProps =
    let empty =
      { actions = []
        query = []
        canRefresh = true
        canSearch = false
        description = None
        fetchFileContent = false }

  type DirInfo =
    { name: string
      path: string
      icon: string option
      props: DirProps option
      items: ExplorerItem list option }
  module DirInfo =
    let empty =
      { name = ""
        path = ""
        icon = None
        props = None
        items = None }


  type ExplorerItem =
    | FileNode of FileInfo
    | DirNode of DirInfo