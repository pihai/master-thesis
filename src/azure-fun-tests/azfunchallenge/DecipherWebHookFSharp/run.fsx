#r "System.Net.Http"
#r "Newtonsoft.Json"

open System.Net
open System.Net.Http
open Newtonsoft.Json

type Req = {
    key : string
    msg : string
    cipher : Map<string, int>
}

let Run(req: HttpRequestMessage, log: TraceWriter) =
    async {
        let! jsonContent = req.Content.ReadAsStringAsync() |> Async.AwaitTask
        try
            let data = JsonConvert.DeserializeObject<Req>(jsonContent)
            let reverseCipher = data.cipher |> Seq.map (fun x -> x.Value, x.Key) |> Map.ofSeq
            let result =
                data.msg 
                |> Seq.chunkBySize 2 
                |> Seq.map (fun [| x; y |] -> System.Int32.Parse(x.ToString() + y.ToString())) 
                |> Seq.map (fun x -> reverseCipher |> Map.find x)
                |> String.concat ""
            let resObj = [ "key", data.key; "result", result ] |> dict
            return req.CreateResponse(HttpStatusCode.OK, resObj)
        with _ ->
            return req.CreateResponse(HttpStatusCode.BadRequest)
    } |> Async.StartAsTask
