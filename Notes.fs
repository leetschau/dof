module Notes

open System
open Config
open System.IO

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
        appConfig.RecordPath,
        List.map (fun note -> note.FilePath) notes
        |> String.concat "\n"
    )
    let header = "No. Updated, Notebook, Title, Created, Tags"
    let body = List.map
                    (fun note ->
                        (note.Updated.ToString "yyyy/MM/dd")
                        + " "
                        + note.Notebook
                        + ": "
                        + note.Title
                        + " ["
                        + (note.Created.ToString "yyyy/MM/dd")
                        + "] "
                        + (note.TagList |> String.concat "; "))
                    notes
    let withIdx = List.zip [1 .. List.length body] body |>
                  List.map (fun (idx, body) -> string idx + ". " + body)

    (header :: withIdx) |> String.concat "\n"


let simpleSearch (args: string list) : string =
    let wordInNote (word: string) (note: Note) : bool =
        (note.Content.Contains word)
        || (note.Title.Contains word)
        || (List.exists (fun (tag: string) -> tag.Contains word) note.TagList)

    let notes =
        List.fold (fun noteList word ->
                       noteList |> List.filter (wordInNote word))
                  (loadNotes appConfig.NoteRepo)
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
            (loadNotes appConfig.NoteRepo)
            (List.map parseTerm args)

    saveAndDisplayNotes notes


let listNotes (num: int) : string =
    ((loadNotes appConfig.NoteRepo
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

    File.WriteAllText(appConfig.TempFile, header)

    let p =
        System.Diagnostics.Process.Start(appConfig.UserConf.Editor,
                                         appConfig.TempFile)

    p.WaitForExit()

    let timestamp =
        System.DateTime.Now.ToString "yyMMddHHmmss"

    let target =
        Path.Combine(appConfig.NoteRepo, $"note{timestamp}.md")

    File.Move(appConfig.TempFile, target)
    listNotes appConfig.DefaultRecNo


let editNote (no: int) : string =
    let path = (File.ReadAllLines appConfig.RecordPath).[no - 1]

    let psi = Diagnostics.ProcessStartInfo(appConfig.UserConf.Editor, path)
    List.iter (fun { Name = name; Value = value } ->
        psi.Environment.Add(name, value) ) appConfig.UserConf.AppEnv
    let p = Diagnostics.Process.Start(psi)
    p.WaitForExit()
    listNotes appConfig.DefaultRecNo


let viewNote (no: int) =
    let path = (File.ReadAllLines appConfig.RecordPath).[no - 1]

    let cmd = (appConfig.UserConf.Viewer + " " + path).Split(" ")
    // there may be whitespace in Viewer, for example `nvim -R`
    let psi = Diagnostics.ProcessStartInfo(cmd.[0],
                                           cmd.[1..] |> String.concat " ")
    List.iter (fun { Name = name; Value = value } ->
        psi.Environment.Add(name, value) ) appConfig.UserConf.AppEnv
    let p = Diagnostics.Process.Start(psi)
    p.WaitForExit()
    (*File.ReadAllText(appConfig.RecordPath)*)
