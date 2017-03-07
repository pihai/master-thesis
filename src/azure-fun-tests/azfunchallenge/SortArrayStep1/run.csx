#r "Newtonsoft.Json"

using System;
using System.Net;
using Newtonsoft.Json;

public class Input {
    public string key { get; set; }
    public int[] ArrayOfValues { get; set; }
}

public class Output {
    public string PartitionKey { get; set; }
    public string RowKey { get; set; }
    public int[] ArrayOfValues { get; set; }
}

public static async Task<object> Run(HttpRequestMessage req, IAsyncCollector<Output> outputTable, TraceWriter log)
{
    log.Info($"Webhook was triggered!");

    var jsonContent = await req.Content.ReadAsStringAsync();
    var data = JsonConvert.DeserializeObject<Input>(jsonContent);

    if (data.key == null || data.ArrayOfValues == null)
        return req.CreateResponse(HttpStatusCode.BadRequest);

    await outputTable.AddAsync(
        new Output { PartitionKey = "sort", RowKey = data.key, ArrayOfValues = data.ArrayOfValues }
    );

    return req.CreateResponse(HttpStatusCode.OK, new {});
}
