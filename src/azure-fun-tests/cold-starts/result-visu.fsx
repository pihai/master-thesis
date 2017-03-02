#r @"..\packages\WindowsAzure.Storage\lib\net45\Microsoft.WindowsAzure.Storage.dll"

open System
open Microsoft.WindowsAzure.Storage
open Microsoft.WindowsAzure.Storage.Table

type TimingLogEntry() =
  class
    inherit TableEntity()
    member val Email : string = "" with get, set
    member val StatusCode : int = 0 with get, set
    member val DurationMillis : int = 0 with get, set
    member val Location : string = "" with get, set
    member val Url : string = "" with get, set
    member x.Foo() = ()
  end

let downlaodTableToCsv cnnStr tableName outFileName =
  let account = CloudStorageAccount.Parse(cnnStr)
  let client = account.CreateCloudTableClient()
  let table = client.GetTableReference tableName
  let tableQ = TableQuery<TimingLogEntry>()
  let results = 
    let rec loop (cont: TableContinuationToken) acc = 
      let result = table.ExecuteQuerySegmented(tableQ, cont)
      let x = acc @ (result.Results |> Seq.toList)
      match result.ContinuationToken with
      | null -> x
      | cont -> loop cont x
    loop null []
  results

let cnnStr = 
  let keyFile = IO.Path.Combine(__SOURCE_DIRECTORY__, "storagekey.secrets")
  if IO.File.Exists keyFile then
    IO.File.ReadAllText keyFile
  else
    failwith "No storage key found. Please run 'download-storage-cnn-str.ps1' first."

let entries = downlaodTableToCsv cnnStr "coldStartTiming" "nix"

entries |> List.length

entries
|> List.groupBy (fun x -> x.Location)
|> List.map (fun (key,rows) ->
  key, rows |> List.averageBy (fun x -> x.DurationMillis |> float)
)