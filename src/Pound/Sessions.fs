
namespace Pound.Sessions

// todo: simple persistent session state
// todo: helpers for tcp/pipes

open System
open Futility
open Newtonsoft.Json.Linq
open FGateway
open FInvoke
open FSerial

type Session = {
  ID                : string
  State             : TypeMap
  FirstRequest      : Uri
  Referrer          : Uri option
  WhenCreated       : DateTime
  WhenLastRequest   : DateTime
  Timeout           : TimeSpan
  Address           : Net.IPAddress
}

type SessionStore = {
  Save              : Session -> unit
  Load              : string -> Session option
  List              : unit -> string list
  Delete            : string -> unit
  Count             : unit -> int
  Timeout           : TimeSpan
  Purge             : unit -> unit
}

[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Session =
  module InMemory =
    let store () =
      let dict = Dict<string, Session>()
      let save so =
        if dict.ContainsKey so.ID then dict.[so.ID] <- so
        else dict.Add (so.ID, so)
      let load id =
        if dict.ContainsKey id then Some dict.[id]
        else None
      let list () = dict.Keys |> List.ofSeq
      let delete id =
        if dict.ContainsKey id then dict.Remove id |> ignore
        else ()
      let count () = dict.Count
      let purge () =
        let now = DateTime.UtcNow
        dict
        |> List.ofSeq
        |> List.filter (fun p -> now - p.Value.WhenLastRequest > p.Value.Timeout)
        |> List.iter (fun p -> dict.Remove p.Key |> ignore)
      { Save = save |> lockf dict
        Load = load |> lockf dict
        List = list |> lockf dict
        Delete = delete |> lockf dict
        Count = count |> lockf dict
        Purge = purge |> lockf dict
        Timeout = TimeSpan.Zero
      }  
  module Remote =
    type SessionObject = {
      ID                : string
      State             : string
      FirstRequest      : Uri
      Referrer          : Uri option
      WhenCreated       : DateTime
      WhenLastRequest   : DateTime
      Timeout           : TimeSpan
      Address           : Net.IPAddress
    }
    type Service = {
      Save              : SessionObject -> unit
      Load              : string -> SessionObject option
      List              : unit -> string list
      Delete            : string -> unit
      Count             : unit -> int
      Purge             : unit -> unit
    }
    let private s2o (s : Session) : SessionObject =
      { ID = s.ID
        State = s.State |> FSerial.node typeof<TypeMap> |> string
        FirstRequest = s.FirstRequest
        Referrer = s.Referrer
        WhenCreated = s.WhenCreated
        WhenLastRequest = s.WhenLastRequest
        Timeout = s.Timeout
        Address = s.Address
      }
    let private o2s (s : SessionObject) : Session =
      { ID = s.ID
        State =
          s.State
          |> JArray.Parse :> JToken
          |> FSerial.ofNode typeof<TypeMap>
          :?> TypeMap
        FirstRequest = s.FirstRequest
        Referrer = s.Referrer
        WhenCreated = s.WhenCreated
        WhenLastRequest = s.WhenLastRequest
        Timeout = s.Timeout
        Address = s.Address
      }
    let client (url : string) sessionTimeout requestTimeout : SessionStore =
      let client : Service = Proxy.http Format.bson requestTimeout url
      let save s = s |> s2o |> client.Save
      let load id =
        match id |> client.Load with 
        | None -> None 
        | Some x -> Some (x |> o2s)
      let list () = client.List ()
      let delete id = client.Delete id
      let count () = client.Count ()
      let purge () = client.Purge ()
      { Save = save
        Load = load
        List = list
        Delete = delete
        Count = count
        Purge = purge
        Timeout = sessionTimeout
      }
    let service (store : SessionStore) =
      let load i =
        match store.Load i with
        | None -> None
        | Some s -> s |> s2o |> Some
      let inst =
        { Save = o2s >> store.Save
          Load = load
          List = store.List
          Delete = store.Delete
          Count = store.Count
          Purge = store.Purge
        }
      Host.http inst Format.bson

  let load (store : SessionStore) (request : Request) =
    let newSession () =
      let id = Guid.NewGuid () |> string
      let now = DateTime.UtcNow
      let c =
        { Name = "s"
          Value = id
          WhenExpires = Some (DateTime.UtcNow.AddYears 5)
          IsHttpOnly = true
          Domain = null
          Path = null
          IsSecure = false
        }
      request.SetCookie c
      { ID = id
        State = TypeMap ()
        FirstRequest = request.Url
        WhenCreated = now
        WhenLastRequest = now
        Referrer = if request.Referrer = null then None else Some request.Referrer
        Timeout = store.Timeout
        Address = request.ClientAddress
      }
    let current =
      match request.Cookies.TryFind "s" with
      | None -> newSession ()
      | Some cookie ->
        match store.Load cookie.Value with
        | None -> newSession ()
        | Some s ->
          let youShallNotPass =
            DateTime.UtcNow - s.WhenLastRequest > s.Timeout
            || s.Address <> request.ClientAddress
          if youShallNotPass then
            newSession ()
          else s
    let session =
      { current with
          WhenLastRequest = DateTime.UtcNow }
    request.State.Set session
    request
  let save (store : SessionStore) (request : Request) =
    store.Save (request.State.Get<Session>().Value)
    request

