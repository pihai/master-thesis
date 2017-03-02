using System;
using System.Diagnostics;

public static async Task Run(TimerInfo myTimer, TraceWriter log, IAsyncCollector<TimingLogEntry> timingEntry) {
    using(var client = new HttpClient()) {
        var sw = Stopwatch.StartNew();
        var url = Environment.GetEnvironmentVariable("url");
        var location = Environment.GetEnvironmentVariable("location");        
        var response = await client.GetAsync(url);
        sw.Stop();

        log.Info(sw.Elapsed.TotalMilliseconds.ToString());
        await timingEntry.AddAsync(new TimingLogEntry {
            PartitionKey = "log",
            RowKey = DateTime.Now.ToString("s"),
            DurationMillis = (int) sw.Elapsed.TotalMilliseconds,
            StatusCode = (int) response.StatusCode,
            Location = location,
            Url = url
        });
    }
}

public class TimingLogEntry {
    public string PartitionKey { get; set; }
    public string RowKey { get; set; }
    public int StatusCode { get; set; }
    public int DurationMillis { get; set; }
    public string Location { get; set; }
    public string Url { get; set; }
}