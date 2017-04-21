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
      var config = Orleans.Runtime.Configuration.ClientConfiguration.LocalhostSilo(30000);
      GrainClient.Initialize(config);

      var greeter = GrainClient.GrainFactory.GetGrain<IHello>(Guid.NewGuid());
      var greeting = greeter.SayHello().GetAwaiter().GetResult();
      Console.WriteLine(greeting);

      Console.Read();
    }
  }
}
