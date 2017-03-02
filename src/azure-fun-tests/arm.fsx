#r @"packages\ARMClient.Authentication\lib\net45\ARMClient.Authentication.dll"
#I @"packages\Microsoft.IdentityModel.Clients.ActiveDirectory\lib\net45"
#r @"Microsoft.IdentityModel.Clients.ActiveDirectory.dll"
//#r @"packages\Microsoft.IdentityModel.Clients.ActiveDirectory\lib\net45\Microsoft.IdentityModel.Clients.ActiveDirectory.WindowsForms.dll"
#r @"packages\Microsoft.Azure.Management.Storage\lib\net45\Microsoft.Azure.Management.Storage.dll"
#r @"packages\Microsoft.Rest.ClientRuntime.Azure\lib\net45\Microsoft.Rest.ClientRuntime.Azure.dll"
#r @"packages\Microsoft.Rest.ClientRuntime\lib\net45\Microsoft.Rest.ClientRuntime.dll"
#r "System.Net.Http"
// #r @"packages\Microsoft.Azure.Management.ResourceManager\lib\net45\Microsoft.Azure.Management.ResourceManager.dll"
// #r @"packages\WindowsAzure.Storage\lib\net45\Microsoft.WindowsAzure.Storage.dll"

open ARMClient.Authentication.AADAuthentication
open Microsoft.Rest
open Microsoft.Azure.Management.ResourceManager
open Microsoft.Azure.Management.Storage
open Microsoft.WindowsAzure.Storage
open Microsoft.WindowsAzure.Storage.Table

let storageResGroup = "Garbage"
let storageAccountName = "hptimingstorage"

let getConnectionString storageName resGrp = 
  let authHelper = PersistentAuthHelper()
  authHelper.AcquireTokens() |> Async.AwaitTask |> Async.RunSynchronously
  let subscription = authHelper.GetTenantsInfo() |> Seq.collect (fun t -> t.subscriptions) |> Seq.head
  let token = subscription.subscriptionId |> authHelper.GetToken |> Async.AwaitTask |> Async.RunSynchronously
  let credential = new TokenCredentials(token.AccessToken)
  // let resourceManagementClient = new ResourceManagementClient(credential,  SubscriptionId = subscription.subscriptionId)
  let storageClient = new StorageManagementClient(credential)
  //storageClient.SubscriptionId <- subscription.subscriptionId
  // let keys = storageClient.StorageAccounts.ListKeys(resGrp, storageName).Keys
  // sprintf "DefaultEndpointsProtocol=https;AccountName=%s;AccountKey=%s" storageAccountName (keys.[0].Value)
  ()

type TimingLogEntry() =
    inherit TableEntity()
    member val Email : string = "" with get, set
    member val StatusCode : int = 0 with get, set
    member val DurationMillis : int = 0 with get, set
    member val Location : string = "" with get, set
    member val Url : string = "" with get, set

let downlaodTableToCsv cnnStr tableName outFileName =
  let account = CloudStorageAccount.Parse(cnnStr)
  let client = account.CreateCloudTableClient()
  let table = client.GetTableReference tableName
  let tableQ = TableQuery<TimingLogEntry>()
  let results = 
    let rec loop (cont: TableContinuationToken) acc = 
      let result = table.ExecuteQuerySegmented(tableQ, cont)
      match result.ContinuationToken with
      | null -> ()
      | cont -> loop cont (acc @ (result.Results |> Seq.toList))
    loop null
  results

let cnnStr = getConnectionString storageResGroup storageAccountName