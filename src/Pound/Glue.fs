
namespace Pound

open System
open System.IO
open Futility
open FGateway
open FMarkup

[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module HtmlView =
  let viewer (printer : HtmlPrinter) (views : HtmlView list) =
    let exec model request =
      let view =
        views
        |> List.tryFind (fun v -> v.InModel == model.GetType ())
      let toPrint =
        match view with
        | None -> model
        | Some view ->
          view.Method.Invoke (null, [| model |])
      request.SetContentType "text/html"
      request.SetContentEncoding Text.UTF8Encoding.UTF8
      let tw = new StreamWriter (request.OutputStream)
      printer.Print tw toPrint
      tw.Flush ()
      true
    Mvc.Viewer exec
