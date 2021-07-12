module Notes

open System
open System.IO
open Microsoft.FSharpLu.Json

let EDITOR = "nvim"
let VIEWER = "nvim -R"
let BASE = "/home/leo/.donno"
let NOTE_REPO = Path.Combine(BASE, "repo")
let REC_PATH = Path.Combine(BASE, "records")
let TEMP_FILE = "newnote.md"
let DEFAULT_REC_NO = 5


type Note =
    { Title: string
      TagList: string list
      Notebook: string
      Created: DateTime
      Updated: DateTime
      Content: string
      FilePath: string }


let parseNote (note: string) : Note =
    let lines = File.ReadAllLines note

    { Title = lines.[0].[7..]
      TagList =
          lines.[1].[6..].Split ";"
          |> Array.toList
          |> List.map (fun (x: string) -> x.Trim())
      Notebook = lines.[2].[10..]
      Created = lines.[3].[9..] |> DateTime.Parse
      Updated = lines.[4].[9..] |> DateTime.Parse
      Content = lines.[8..] |> String.concat "\n"
      FilePath = note }


let loadNotes (path: string) : Note list =
    let files =
        Directory.GetFiles(path, "*.md") |> Array.toList

    List.map parseNote files


let saveAndDisplayNotes (notes: Note list) : string =
    File.WriteAllText(
        REC_PATH,
        List.map (fun note -> note.FilePath) notes
        |> String.concat "\n"
    )

    let header = "Resuult:"

    let body =
        List.map
            (fun note ->
                (note.Updated.ToString "yyyy/MM/dd")
                + ", "
                + note.Title
                + ", "
                + (note.TagList |> String.concat "; ")
                + ", "
                + (note.Created.ToString "yyyy/MM/dd"))
            notes

    (header :: body) |> String.concat "\n"


let simpleSearch (args: string list) : string =
    let wordInNote (word: string) (note: Note) : bool =
        (note.Content.Contains word)
        || (note.Title.Contains word)
        || (List.exists (fun (tag: string) -> tag.Contains word) note.TagList)

    let notes =
        List.fold (fun noteList word ->
                       noteList |> List.filter (wordInNote word))
                  (loadNotes NOTE_REPO)
                  args

    saveAndDisplayNotes notes


type SearchItem =
    | Title of string
    | Tag of string
    | Notebook of string
    | Created of DateTime
    | Updated of DateTime
    | Content of string

type SearchFlag =
    | TextFlag of ignoreCase: bool * wholeWord: bool
    | DateFlag of beforeDate: bool

type SearchTerm =
    { Body: SearchItem
      Flag: SearchFlag option }


let advancedSearch (args: string list) : string =
    let parseTerm (term: string) : SearchTerm option =
        let terms = term.Split(":") |> Array.toList

        let baseTerm: SearchTerm option =
            match terms with
            | [ "ti"; word ]
            | [ "ti"; word; _ ] -> Some({ Body = Title(word); Flag = None })
            | [ "ta"; word ]
            | [ "ta"; word; _ ] -> Some({ Body = Tag(word); Flag = None })
            | [ "nb"; word ]
            | [ "nb"; word; _ ] -> Some({ Body = Notebook(word); Flag = None })
            | [ "cr"; date ]
            | [ "cr"; date; _ ] ->
                Some({ Body = Created(DateTime.Parse date); Flag = None })
            | [ "up"; date ]
            | [ "up"; date; _ ] ->
                Some({ Body = Updated(DateTime.Parse date); Flag = None })
            | t ->
                printfn "Invalid search term: %A" t
                None

        match baseTerm with
        | Some (baseT) & (Some ({ Body = Title (_); Flag = _ })
                         | Some ({ Body = Tag (_); Flag = _ })
                         | Some ({ Body = Notebook (_); Flag = _ })) ->
            match terms with
            | [ _; _ ]
            | [ _; _; "iW" ]
            | [ _; _; "Wi" ]
            | [ _; _; "i" ]
            | [ _; _; "W" ] ->
                Some({ baseT with
                         Flag = Some(TextFlag(ignoreCase = true,
                                              wholeWord = false)) }
                )
            | [ _; _; "iw" ]
            | [ _; _; "wi" ]
            | [ _; _; "w" ] ->
                Some({ baseT with
                         Flag = Some(TextFlag(ignoreCase = true,
                                              wholeWord = true)) }
                )
            | [ _; _; "Iw" ]
            | [ _; _; "wI" ] ->
                Some({ baseT with
                         Flag = Some(TextFlag(ignoreCase = false,
                                              wholeWord = true)) }
                )
            | [ _; _; "IW" ]
            | [ _; _; "WI" ]
            | [ _; _; "I" ] ->
                Some({ baseT with
                         Flag = Some(TextFlag(ignoreCase = false,
                                              wholeWord = false)) }
                )
            | _ -> None
        | Some (baseT) & (Some ({ Body = Created (_); Flag = _ })
                         | Some ({ Body = Updated (_); Flag = _ })) ->
            match terms with
            | [ _; _; "b" ] ->
                Some(
                    { baseT with
                          Flag = Some(DateFlag(beforeDate = true)) }
                )
            | [ _; _; "B" ] ->
                Some(
                    { baseT with
                          Flag = Some(DateFlag(beforeDate = false)) }
                )
            | [ _; _; flag ] ->
                printfn "Invalid search style: %s" flag
                Some({ baseT with Flag = None })
            | _ -> None
        | _ -> None

    let noteOnTerm (term: SearchTerm option) (note: Note) : bool =
        match term with
        | Some ({ Body = Title (word)
                  Flag = Some (TextFlag (icase, wword)) }) ->
            let target =
                if icase then
                    note.Title.ToLower()
                else
                    note.Title

            let token = if icase then word.ToLower() else word

            if wword then
                target.Split " "
                |> Array.toList
                |> List.exists (fun x -> x = token)
            else
                target.Contains token
        | Some ({ Body = Tag (tag)
                  Flag = Some (TextFlag (icase, wword)) }) ->
            let target =
                List.map (fun (x: string) -> if icase then x.ToLower() else x)
                         note.TagList

            let token = if icase then tag.ToLower() else tag

            if wword then
                List.exists (fun x -> x = token) target
            else
                List.exists (fun (x: string) -> x.Contains token) target
        | Some ({ Body = Notebook (notebook)
                  Flag = Some (TextFlag (icase, wword)) }) ->
            let target =
                if icase then
                    note.Notebook.ToLower()
                else
                    note.Notebook

            let token =
                if icase then
                    notebook.ToLower()
                else
                    notebook

            if wword then
                target = token
            else
                target.Contains token
        | Some ({ Body = Created (created)
                  Flag = Some (DateFlag (beforeDate = bd)) }) ->
            if bd then
                note.Created < created
            else
                note.Created >= created
        | Some ({ Body = Updated (updated)
                  Flag = Some (DateFlag (beforeDate = bd)) }) ->
            if bd then
                updated <= note.Updated
            else
                updated > note.Updated
        | _ -> false

    let notes =
        List.fold
            (fun noteList term -> noteList |> List.filter (noteOnTerm term))
            (loadNotes NOTE_REPO)
            (List.map parseTerm args)

    saveAndDisplayNotes notes


let listNotes (num: int) : string =
    ((loadNotes NOTE_REPO
      |> List.sortByDescending (fun note -> note.Updated))).[..num - 1]
    |> saveAndDisplayNotes


let addNote () : string =
    let created =
        System.DateTime.Now.ToString "yyyy-MM-dd HH:mm:ss"

    let header =
        $"Title: \n\
                   Tags: \n\
                   Notebook: \n\
                   Created: {created}\n\
                   Updated: {created}\n\n\
                   ------\n\n"

    File.WriteAllText(TEMP_FILE, header)

    let p =
        System.Diagnostics.Process.Start(EDITOR, TEMP_FILE)

    p.WaitForExit()

    let timestamp =
        System.DateTime.Now.ToString "yyMMddHHmmss"

    let target =
        Path.Combine(NOTE_REPO, $"note{timestamp}.md")

    File.Move(TEMP_FILE, target)
    listNotes DEFAULT_REC_NO


let editNote (no: int) : string =
    let path = (File.ReadAllLines REC_PATH).[no - 1]

    let p =
        System.Diagnostics.Process.Start(EDITOR, path)

    p.WaitForExit()
    listNotes DEFAULT_REC_NO


let viewNote (no: int) : string =
    let path = (File.ReadAllLines REC_PATH).[no - 1]
    let cmd = (VIEWER + " " + path).Split(" ")
    // there may be whitespace in VIEWER, for example `nvim -R`

    let p = System.Diagnostics.Process.Start(cmd.[0],
                                             cmd.[1..] |> String.concat " ")

    p.WaitForExit()
    File.ReadAllText(REC_PATH)
