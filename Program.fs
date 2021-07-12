open System
open System.Reflection

let usage = """Usage:
search:
edit:
add:
backup:
"""

[<EntryPoint>]
let main argv =
  match argv |> Array.toList with
  | ["a"] | ["add"] ->
    printfn "%s" (Notes.addNote ())
  | ["e"] | ["edit"] ->
    printfn "%s" (Notes.editNote 1)
  | ["e"; no] | ["edit"; no] ->
    printfn "%s" (no |> int |> Notes.editNote)
  | ["l"] | ["list"] ->
    printfn "%s" (Notes.listNotes Notes.DEFAULT_REC_NO)
  | ["l"; num] | ["list"; num] ->
    printfn "%s" (num |> int |> Notes.listNotes)
  | "s" :: "-a" :: args | "search" :: "--advanced" :: args ->
    printfn "%s" (Notes.advancedSearch args)
  | "s" :: args | "search" :: args ->
    printfn "%s" (Notes.simpleSearch args)
  | ["v"] | ["view"] ->
    printfn "%s" (Notes.viewNote 1)
  | ["v"; no] | ["view"; no] ->
    printfn "%s" (no |> int |> Notes.viewNote)
  | ["version"] ->
    printfn "donno version: %s" <| Assembly.GetEntryAssembly().GetName().Version.ToString()
  | [] | _ ->
    printf "%s" usage
  0
