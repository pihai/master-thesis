using GrainInterfaces;
using Orleans;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Client
{
  class Program
  {
    static void Main(string[] args)
    {
      Console.WriteLine("Waiting for Orleans Silo to start. Press Enter to proceed...");
      Console.ReadLine();

      // Orleans comes with a rich XML and programmatic configuration. Here we're just going to set up with basic programmatic config
      var config = Orleans.Runtime.Configuration.ClientConfiguration.LoadFromFile("ClientConfiguration.xml");
      //var config = Orleans.Runtime.Configuration.ClientConfiguration.LocalhostSilo(30000);
      GrainClient.Initialize(config);

      // DoSensorAsync().GetAwaiter().GetResult();
      //DoCountAsync().GetAwaiter().GetResult();
      //DoPersistentCountAsync().GetAwaiter().GetResult();
      //DoTimeoutTestAsync().GetAwaiter().GetResult();
      //DoQueueTestAsync().GetAwaiter().GetResult();
      //DoDeactivationTest().GetAwaiter().GetResult();
      //DoReentrancyTestAsync().GetAwaiter().GetResult();
      //DoGrainCommunicationTest().GetAwaiter().GetResult();
      DoMaximalKeySizeTestAsync().GetAwaiter().GetResult();
      Console.Read();

    }

    static async Task DoSensorAsync()
    {
      var random = new Random();
      var sensorId = Guid.Parse("eeb48102-c52f-4929-b7b3-da7cce96503f");
      var sensor = GrainClient.GrainFactory.GetGrain<ISensor>(sensorId);
      Console.WriteLine(sensor.GetType());

      for(var i = 0; i < 10; i++)
        await sensor.AddMeasure(random.NextDouble());
      Console.WriteLine($"Avg: {await sensor.GetAverage()}");
    }

    static async Task DoCountAsync() {
      var counter = GrainClient.GrainFactory.GetGrain<ICounter>(0);
      do
      {
        Console.WriteLine($"Count: {await counter.GetCount()}");
        await counter.Increment();
      } while (Console.ReadLine() != "exit");
    }

    static async Task DoPersistentCountAsync()
    {
      var counter = GrainClient.GrainFactory.GetGrain<IPersistentCounter>(0);
      do
      {
        try
        {
          Console.WriteLine($"Count: {await counter.GetCount()}");
        } catch(Exception e)
        {
          Console.WriteLine(e);
        }
        await counter.Increment();
      } while (Console.ReadLine() != "exit");
    }

    static async Task DoTimeoutTestAsync() {
      var queue = GrainClient.GrainFactory.GetGrain<IQueue>(0);
      var t1 = queue.ProcessAsync(TimeSpan.FromMinutes(1.1));
      await queue.ProcessAsync(TimeSpan.FromSeconds(35));
      await t1;
      Console.WriteLine("done.");
    }

    static async Task DoQueueTestAsync() {
      var queue = GrainClient.GrainFactory.GetGrain<IQueue>(0);
      Enumerable.Range(0, 100).ToList().ForEach(_ => queue.ProcessAsync(TimeSpan.FromSeconds(1)));
      Console.WriteLine("done.");
    }

    static async Task DoDeactivationTest()
    {
      try
      {
        var grain = GrainClient.GrainFactory.GetGrain<IDeactivationTest>(0);
        await grain.Incr();
        Console.WriteLine($"Count = {await grain.GetCount()}");
        Console.WriteLine("Wait until the silo says deactivated ...");
        Console.Read();
        Console.WriteLine($"Count = {await grain.GetCount()}");
        Console.WriteLine("done.");
      }
      catch (Exception e)
      {
        Console.WriteLine(e);
      }
    }

    static async Task DoReentrancyTestAsync()
    {
      var queue = GrainClient.GrainFactory.GetGrain<IReentrantQueue>(0);
      var t1 = queue.ProcessAsync(TimeSpan.FromMinutes(1.1));
      await queue.ProcessAsync(TimeSpan.FromSeconds(35));
      await t1;
      Console.WriteLine("done.");
    }

    static async Task  DoGrainCommunicationTest()
    {
      var caller = GrainClient.GrainFactory.GetGrain<ICaller>(0);
      var callee = GrainClient.GrainFactory.GetGrain<ICallee>(0);
      await caller.CallAsync(callee);
      Console.WriteLine("done.");
    }

    static async Task DoMaximalKeySizeTestAsync()
    {
      var id = new String('a', 512);
      var grain = GrainClient.GrainFactory.GetGrain<IMaximalKeySize>(id);
      await grain.Foo();

    }
  }
}
