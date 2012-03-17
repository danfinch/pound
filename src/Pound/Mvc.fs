
namespace Pound.Mvc

// todo: convert more primitive/bcl types
// todo: simple values as operation parameters
// todo: too many guts exposed, behavior not tweakable
// todo: don't require controller root as domain root
// todo: general purpose rest controller

open System
open System.IO
open System.Reflection
open Microsoft.FSharp.Reflection
open Futility
open FGateway

exception HttpError of int
exception ValidationError of string // move to FModel

type Viewer = Viewer of (obj -> Request -> bool)
type Redirect = Redirect of string
type CustomAction = CustomAction of (Request -> unit)
type PublishAttribute () = inherit System.Attribute ()
type InputSource = Path | Query | Form
type SourceAttribute (s : InputSource) = inherit Attribute (); member self.Source = s
type PathAttribute () = inherit SourceAttribute (Path)
type QueryAttribute () = inherit SourceAttribute (Query)
type FormAttribute () = inherit SourceAttribute (Form)

type Method =
  | Head | Get | Post | Put | Delete
  | Trace | Options | Connect | Patch
  | Other of string
with
  static member parse s =
    match s |> String.upper with
    | "HEAD" -> Head | "GET" -> Get
    | "POST" -> Post | "PUT" -> Put
    | "DELETE" -> Delete
    | "TRACE" -> Trace | "OPTIONS" -> Options
    | "CONNECT" -> Connect | "PATCH" -> Patch
    | s -> Other (s.ToUpper ())
  override self.ToString () =
    match self with
    | Head -> "HEAD" | Get -> "GET"
    | Post -> "POST" | Put -> "PUT"
    | Trace -> "TRACE" | Options -> "OPTIONS"
    | Connect -> "CONNECT" | Patch -> "PATCH"
    | Delete -> "DELETE" | Other s -> s

[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Method =
  let parse s =
    match s |> String.upper with
    | "HEAD" -> Head | "GET" -> Get
    | "POST" -> Post | "PUT" -> Put
    | "DELETE" -> Delete
    | "TRACE" -> Trace | "OPTIONS" -> Options
    | "CONNECT" -> Connect | "PATCH" -> Patch
    | s -> Other (s.ToUpper ())

type Input = {
  Name            : string
  Type            : Type
  Source          : InputSource
  Config          : TypeMap
  Field           : PropertyInfo
}

type Operation = {
  Method          : Method
  InModel         : Type option
  OutModel        : Type option
  Inputs          : Input list
  Invoke          : obj -> obj
  Config          : TypeMap
}

type Controller = {
  Path            : string
  Controllers     : Controller list
  Operations      : Operation list
  Config          : TypeMap
}

[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Input =
  module private Parse =
    let bool ss =
      match ss |> List.head |> String.upper with
      | "TRUE" | "ON" | "YES" | "1" -> true
      | _ -> false
      :> obj
    let int ss =
      match ss |> List.head |> Int32.tryParse with
      | Some x -> x :> obj
      | None -> raise (ValidationError "Invalid number.")
    let int64 ss =
      match ss |> List.head |> Int64.tryParse with
      | Some x -> x :> obj
      | None -> raise (ValidationError "Invalid number.")
    let single ss =
      match ss |> List.head |> Single.tryParse with
      | Some x -> x :> obj
      | None -> raise (ValidationError "Invalid number.")
    let double ss =
      match ss |> List.head |> Double.tryParse with
      | Some x -> x :> obj
      | None -> raise (ValidationError "Invalid number.")
    let decimal ss =
      match ss |> List.head |> Decimal.tryParse with
      | Some x -> x :> obj
      | None -> raise (ValidationError "Invalid number.")
  let private oneCtor (t : Type) pt =
    let par (p : ParameterInfo) =
      p.ParameterType = pt
    let con (c : ConstructorInfo) =
      c.GetParameters ()
      |> Array.exists par
    t.GetConstructors ()
    |> Array.exists con
    |> fun b -> if b then Some t else None
  let private (|HasStringCtor|_|) (t : Type) =
    oneCtor t typeof<string>
  let private (|HasStringListCtor|_|) (t : Type) =
    oneCtor t typeof<string list>
  let rec fromList (inputType : Type) (values : string list) =
    let values = values |> List.filter (String.IsNullOrEmpty >> not)
    let join (ss : string list) = ss |> String.join ","
    match inputType, values with
    | OptionType et, [] -> null
    | OptionType et, vs ->
      let v = fromList et vs
      typedefof<_ option>
      |> Type.makeGeneric [et]
      |> Type.createInstance [v]
    | _ , [] -> raise (ValidationError "Required.")
    | t, vs when t == typeof<string> -> vs |> join :> obj
    | t, vs when t = typeof<bool> -> vs |> Parse.bool
    | t, vs when t = typeof<int> -> vs |> Parse.int
    | t, vs when t = typeof<int64> -> vs |> Parse.int64
    | t, vs when t = typeof<single> -> vs |> Parse.single
    | t, vs when t = typeof<float> -> vs |> Parse.double
    | t, vs when t = typeof<decimal> -> vs |> Parse.decimal
    // json, timespan, datetime, guid, etc
    | HasStringCtor t, vs -> t |> Type.createInstance [ vs |> join :> obj ]
    | HasStringListCtor t, vs -> t |> Type.createInstance [ vs :> obj ]
    | _ -> failwith "Unsupported type."
  and fromMap (inputType : Type) (inputName : string) (values : Map<string, string list>) : obj =
    match inputType with
    | t when FSharpType.IsRecord t ->
      t
      |> FSharpRecord.getFields
      |> List.map (fun f -> fromMap f.PropertyType (inputName + "." + f.Name) values)
      |> FSharpRecord.make t
    | _ ->
      if values |> Map.containsKey inputName then values.[inputName]
      else []
      |> fromList inputType
  let fromRequest (request : Request) (op : Operation) : obj =
    let fval input =
      let p, source = input.Field, input.Source
      let uname = p.Name |> String.upper
      match source with
      | Query -> request.Query |> fromMap p.PropertyType p.Name
      | Form -> request.Form |> fromMap p.PropertyType p.Name
      | Path ->
        let segs =
          request.Path
          |> String.trimRightAny ['/']
          |> String.split ['/']
        let inputs =
          op.Inputs
          |> List.filter (fun i -> i.Source = Path)
        let mapi s i =
          i.Name
          , match s with
            | s when s >= segs.Length -> []
            | s when s = inputs.Length - 1 -> segs |> List.skip s
            | s -> [ segs.[s] ]
        inputs
        |> List.mapi mapi
        |> Map.ofList
        |> fromMap p.PropertyType p.Name
    match op.InModel with
    | None -> null
    | Some m ->
      op.Inputs
      |> List.map fval
      |> FSharpRecord.make m

[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Operation =
  let path root (f : 'a -> 'b) =
    let filter c = c.Operations |> List.exists (fun o -> match o.OutModel with None -> false | Some i -> i = typeof<'b>)
    let rec find c ans =
      [ for s in c.Controllers do
          yield! find s (ans @ [c])
        if filter c then yield ans @ [c]
      ]
    match find root [] with
    | [ c ] ->
      let fold acc c =
        acc + "/" + c.Path
      match (c |> List.tail |> List.fold fold "") with
      | "" -> "/"
      | x -> x
    | _ -> failwith "Could not find a unique operation with a matching OutModel."

[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Controller =
  let private operation (m : MethodInfo) =
    let pars = m |> Method.getParameters
    let hm =
      if m.IsSpecialName then
        m.Name |> String.skip 4 |> Method.parse
      else
        m.Name |> Method.parse
    { Config = TypeMap ()
      Method = hm
      Inputs =
        match pars with
        | [] -> []
        | [ p ] when p.ParameterType == typeof<Unit> -> []
        | [ p ] ->
          let props =
            p.ParameterType
            |> Type.getProperties
            |> List.sortBy (fun p -> p.MetadataToken)
          let map (p : PropertyInfo) =
            { Name = p.Name
              Type = p.PropertyType
              Config = TypeMap ()
              Field = p
              Source =
                if p.IsDefined (typeof<SourceAttribute>, true) then
                  (p |> Attribute.pick<SourceAttribute>).Source
                else
                  match hm with
                  | Head | Get | Options | Trace ->
                    if props.Length < 2 then
                      Path
                    else
                      Query
                  | _ -> Form
            }
          props
          |> List.map map
        | _ -> failwith "Operation has more than one parameter."
      InModel =
        match pars with
        | [] -> None
        | [ p ] when p.ParameterType == typeof<Unit> -> None
        | [ p ] -> Some p.ParameterType
        | _ -> failwith "Operation has more than one parameter."
      OutModel = if m.ReturnType == typeof<Void> then None else Some m.ReturnType
      Invoke =
        fun im ->
          match pars with
          | [] -> m.Invoke (null, [| |])
          | [ imp ] ->
            if imp == typeof<Unit> then
              m.Invoke (null, [| null |])
            else
              m.Invoke (null, [| im |])
          | _ -> failwith "Operation has more than one parameter."
    }
  let private fromModule ct =
    let rec con (ct : Type) =
      { Path =
          ct.Name
          |> String.replace "--" "."
        Config = TypeMap ()
        Operations =
          ct.GetMethods ()
          |> List.ofArray
          |> List.filter Method.isStatic
          |> List.map operation
        Controllers =
          ct.GetNestedTypes ()
          |> List.ofArray
          |> List.filter FSharpType.IsModule
          |> List.map con
      }
    con ct
  let fromAssembly (assembly : Assembly) =
    let isTop t =
      FSharpType.IsModule t
      && t.IsDefined (typeof<PublishAttribute>, false)
    let cs =
      assembly.GetTypes ()
      |> List.ofArray
      |> List.filter isTop
      |> List.map fromModule
    let root, subs =
      cs
      |> List.partition (fun c -> c.Path |> String.upper = "ROOT")
    match root with
    | [] -> failwith "Could not find root controller."
    | [ root ] ->
      { root with
          Controllers = root.Controllers @ subs
      }
    | _ -> failwith "Multiple root controllers defined."
  let private findWithPath (path : string) (controller : Controller) : (Controller * string) option =
    let segs = path |> String.trimRightAny ['/'] |> String.split ['/']
    let join segs = segs |> String.join "/"
    let rec search controller segs =
      match segs with
      | [] -> Some (controller, "")
      | seg :: rem ->
        match controller.Controllers with
        | [] -> Some (controller, join segs)
        | subs ->
          match subs |> List.tryFind (fun c -> c.Path = seg) with
          | None -> Some (controller, join segs)
          | Some con ->
            search con rem
    search controller segs
  let find (path : string) (controller : Controller) =
    match findWithPath path controller with
    | Some (x, y) -> Some x
    | _ -> None
  let handle (viewers : Viewer list) (root : Controller) (request : Request) : Request =
    let con, rpath =
      match findWithPath request.Path root with
      | None -> raise (HttpError 404)
      | Some x -> x
    let meth = request.Method |> Method.parse
    let op =
      con.Operations
      |> List.tryFind (fun o -> o.Method = meth)
    match op with
    | None -> raise (HttpError 405)
    | Some op ->
      let pathInvalid =
        rpath |> String.IsNullOrEmpty |> not
        && op.Inputs |> List.forall (fun i -> i.Source <> Path)
      if pathInvalid then
        raise (HttpError 404)
      else
        let request = { request with Path = rpath }
        try
          let input = op |> Input.fromRequest request
          let result = op.Invoke input
          match result with
          | null -> request
          | :? Redirect as r ->
            match r with Redirect r -> request.Redirect r
            request
          | :? CustomAction as a ->
            match a with CustomAction a -> a request
            request
          | _ ->
            viewers
            |> Seq.find (fun (Viewer vr) -> vr result request)
            |> ignore
            request
        with
        | HttpError e ->
          request.SetStatusCode e
          request
