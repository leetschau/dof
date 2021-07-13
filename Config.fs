module Config

open System
open System.IO
open Microsoft.FSharpLu.Json

type CustomEnv =
    { Name: string
      Value: string }

type UserConfig =
    { AppHome: string
      DefaultNotebook: string
      Editor: string
      Viewer: string
      AppEnv: CustomEnv list}

type AppConfig =
    { UserConf: UserConfig
      DefaultRecNo: int
      TempFile: string
      NoteRepo: string
      RecordPath: string }

let HomePath = Environment.GetEnvironmentVariable("HOME")
let ConfigFile = Path.Combine(HomePath, ".config/dof/config.json")

let defaultConf = {
    AppHome = Path.Combine(HomePath, ".donno")
    DefaultNotebook = "/Diary/2021"
    Editor = "nvim"
    Viewer = "nvim -R"
    AppEnv = [{ Name = "XDG_CONFIG_HOME";
               Value = "/home/leo/.config/vimrcs/text" }]}

let appConfig =
    let userConf =
        if File.Exists(ConfigFile)
        then Compact.deserializeFile<UserConfig> ConfigFile
        else Directory.CreateDirectory(
                 Path.GetDirectoryName(ConfigFile)) |> ignore
             Compact.serializeToFile ConfigFile defaultConf
             defaultConf

    { UserConf = userConf
      DefaultRecNo = 5
      TempFile = "/tmp/newnote.md"
      NoteRepo = Path.Combine(userConf.AppHome, "repo")
      RecordPath = Path.Combine(userConf.AppHome, "records") }

let get_conf (key: string): string =
    match key with
    | "." -> Compact.serialize appConfig.UserConf
    | "AppHome" -> appConfig.UserConf.AppHome
    | "DefaultNotebook" -> appConfig.UserConf.DefaultNotebook
    | "Editor" -> appConfig.UserConf.Editor
    | "Viewer" -> appConfig.UserConf.Viewer
    | "AppEnv" -> Compact.serialize appConfig.UserConf.AppEnv
    | _ -> "Invalid key: " + key

let set_conf (key: string) (value: string): string =
    let updatedConf =
        match key with
        | "AppHome" -> { appConfig.UserConf with AppHome = value }
        | "DefaultNotebook" ->
            { appConfig.UserConf with DefaultNotebook = value }
        | "Editor" -> { appConfig.UserConf with Editor = value }
        | "Viewer" -> { appConfig.UserConf with Viewer = value }
        | keyname when keyname.StartsWith("AppEnv.") ->
            { appConfig.UserConf with
                AppEnv = { Name = key;
                           Value = value} :: appConfig.UserConf.AppEnv}
        | _ ->
            printfn "Invalid key: %s" key
            appConfig.UserConf

    Compact.serializeToFile ConfigFile updatedConf
    value

