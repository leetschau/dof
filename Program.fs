open System
open Config
open System.Reflection

let usage = """Usage:
s [-a]: search notes
e <n>: edit the <n>th note
v <n>: view the <n>th note
a: add a new note
b: backup note repo
conf [get/set]: get/set config
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
        printfn "%s" (Notes.listNotes appConfig.DefaultRecNo)
    | ["l"; num] | ["list"; num] ->
        printfn "%s" (num |> int |> Notes.listNotes)
    | "s" :: "-a" :: args | "search" :: "--advanced" :: args ->
        printfn "%s" (Notes.advancedSearch args)
    | "s" :: args | "search" :: args ->
        printfn "%s" (Notes.simpleSearch args)
    | ["v"] | ["view"] -> Notes.viewNote 1
    | ["v"; no] | ["view"; no] -> (int no |> Notes.viewNote)
    | ["version"] ->
        printfn "donno version: %s" <|
            Assembly.GetEntryAssembly().GetName().Version.ToString()
    | ["conf"; "get"] ->
        printfn "%s" (get_conf ".")
    | ["conf"; "get"; key] ->
        printfn "%s" (get_conf key)
    | ["conf"; "set"; key; value] ->
        printfn "Set configuration %s to: %s" key (set_conf key value)
    | [] | _ ->
        printf "%s" usage
    0
