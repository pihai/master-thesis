namespace Test

open System
open System.Runtime.InteropServices
open Microsoft.Azure.WebJobs
open Microsoft.Azure.WebJobs

module Functions = 
  type SensorDate = { SensorId: string; Timestamp: string; Value: double }
  let processSensorData 
    ([<QueueTrigger("sensorqueue")>] sensorData : SensorDate,
     [<Blob("values/{SensorId}/{Timestamp}"); Out>] sensorValue : string byref,
     [<Queue("aggregationqueue"); Out>] aggregationQueue : SensorDate byref) = 
     sensorValue <- string sensorData.Value
     aggregationQueue <- sensorData

module Program =
  let [<EntryPoint>] main args = 
    (new JobHost()).Start()
    Console.Read() |> fun _ -> 0
