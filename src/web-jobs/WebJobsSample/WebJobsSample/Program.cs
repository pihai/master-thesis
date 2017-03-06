using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.Azure.WebJobs;

namespace WebJobsSample {
  public class SensorData {
    public string SensorId { get; set; }
    public string Timestamp { get; set; }
    public double Value { get; set; }
  }
  class Program {
    static void Main() => new JobHost().RunAndBlock();
  }
  public class Functions {
    public static void ProcessSensorData(
      [QueueTrigger("sensorqueue")] SensorData sensorData,
      [Blob("values/{SensorId}/{Timestamp}")] out string sensorValue,
      [Queue("aggregationqueue")] out SensorData aggregationQueue
    ) {
      sensorValue = sensorData.Value.ToString();
      aggregationQueue = sensorData;
    }
  }
}
