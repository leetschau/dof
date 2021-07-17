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


let saveNote (note: Note) : string =
    let date2Str (date: DateTime) = date.ToString "yyyy-MM-dd HH:mm:ss"

    let tags = note.TagList |> String.concat "; "
    let res = $"Title: {note.Title}\n\
                Tags: {tags}\n\
                Notebook: {note.Notebook}\n\
                Created: {date2Str note.Created}\n\
                Updated: {date2Str note.Updated}\n\n\
                ------\n\n\
                {note.Content}"

    File.WriteAllText(note.FilePath, res)
    note.FilePath


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


let saveAndDisplayList (notes: Note list) : string =
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

    saveAndDisplayList (
        List.fold (fun noteList word ->
                       noteList |> List.filter (wordInNote word))
                  (loadNotes appConfig.NoteRepo)
                  args
        |> List.sortBy (fun note -> note.Updated) |> List.rev)


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
            | [ "co"; word ]
            | [ "co"; word; _ ] -> Some({ Body = Content(word); Flag = None })
            | t ->
                printfn "Invalid search term: %A" t
                None

        match baseTerm with
        | Some (baseT) & (Some ({ Body = Title (_); Flag = _ })
                         | Some ({ Body = Tag (_); Flag = _ })
                         | Some ({ Body = Content (_); Flag = _ })
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
        | Some ({ Body = Content (word)
                  Flag = Some (TextFlag (icase, wword)) }) ->
            let target =
                if icase then
                    note.Content.ToLower()
                else
                    note.Content

            let token = if icase then word.ToLower() else word

            if wword then
                target.Split " "
                |> Array.toList
                |> List.exists (fun x -> x = token)
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

    saveAndDisplayList (
        List.fold
            (fun noteList term -> noteList |> List.filter (noteOnTerm term))
            (loadNotes appConfig.NoteRepo)
            (List.map parseTerm args)
        |> List.sortBy (fun note -> note.Updated) |> List.rev)


let listNotes (num: int) : string =
    ((loadNotes appConfig.NoteRepo
      |> List.sortByDescending (fun note -> note.Updated))).[..num - 1]
    |> saveAndDisplayList


let runShellCmd (cmd: string) (args: string list) (wordDir: string) =
    // there may be whitespace in `cmd`, for example `nvim -R`
    // also there may be whitespaces in one element of `args`,
    // for example the 3rd element in ["commit", "-m", "'some commit message'"]
    // So we can't simply split command by whitespaces

    let command, arguments =
        match cmd.Split(" ") |> Array.toList with
        | [ rcmd ] -> (rcmd, args)
        | rcmd :: arglist -> (rcmd, arglist @ args)
        | _ -> ("", [""])

    let psi = Diagnostics.ProcessStartInfo(command)
    List.iter (fun arg -> psi.ArgumentList.Add(arg) ) arguments
    List.iter (fun { Name = name; Value = value } ->
        psi.Environment.Add(name, value) ) appConfig.UserConf.AppEnv
    psi.WorkingDirectory <- wordDir
    Diagnostics.Process.Start(psi).WaitForExit()


let addNote () : string =
    let templ =
        { Title = ""
          TagList = [""]
          Notebook = appConfig.UserConf.DefaultNotebook
          Created = DateTime.Now
          Updated = DateTime.Now
          Content = ""
          FilePath = appConfig.TempFile }

    saveNote templ |> ignore

    runShellCmd appConfig.UserConf.Editor [templ.FilePath] ""

    let timestamp = DateTime.Now.ToString "yyMMddHHmmss"
    let target = Path.Combine(appConfig.NoteRepo, $"note{timestamp}.md")

    saveNote { (parseNote templ.FilePath) with
                  Updated = DateTime.Now; FilePath = target }


let editNote (no: int) : string =
    let path = (File.ReadAllLines appConfig.RecordPath).[no - 1]
    saveNote {(parseNote path) with FilePath = appConfig.TempFile } |> ignore

    runShellCmd appConfig.UserConf.Editor [appConfig.TempFile] ""

    saveNote {(parseNote appConfig.TempFile) with
                  Updated = DateTime.Now; FilePath = path }


let viewNote (no: int) =
    let path = (File.ReadAllLines appConfig.RecordPath).[no - 1]
    runShellCmd appConfig.UserConf.Viewer [path] ""


let backupDryRun =
    runShellCmd "git" ["status"] appConfig.NoteRepo


let backup (message: string) =
    runShellCmd "git" [ "add"; "-A" ] appConfig.NoteRepo
    runShellCmd "git" [ "commit"; "-m"; message ] appConfig.NoteRepo
    runShellCmd "git" [ "push"; "origin" ] appConfig.NoteRepo

let preview (no: int) =
    let previewFile =  "/tmp/preview.html"
    let path = (File.ReadAllLines appConfig.RecordPath).[no - 1]
    runShellCmd "pandoc" ["--filter"; "mermaid-filter";
        "--mathjax"; "--toc"; "--standalone";
        "--output"; previewFile; path] ""
    let psi = Diagnostics.ProcessStartInfo(previewFile)
    psi.UseShellExecute <- true
    System.Diagnostics.Process.Start(psi).WaitForExit()

