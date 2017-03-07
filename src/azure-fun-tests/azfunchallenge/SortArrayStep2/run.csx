#r "Newtonsoft.Json"

using System;
using System.Net;
using Newtonsoft.Json;

public class HttpRequest {
    public string key { get; set; }
}

public class TableInput {
    public string PartitionKey { get; set; }
    public string RowKey { get; set; }
    public int[] ArrayOfValues { get; set; }
}

public static object Run(HttpRequest reqData, HttpRequestMessage req, TableInput input, TraceWriter log)
{
    return req.CreateResponse(HttpStatusCode.OK, new {
        key = reqData.key, ArrayOfValues = input.ArrayOfValues.OrderBy(x => x).ToArray()
    });
}
