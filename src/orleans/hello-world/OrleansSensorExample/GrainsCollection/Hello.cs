using GrainInterfaces;
using Orleans;
using Orleans.Concurrency;
using Orleans.Providers;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace GrainsCollection
{
  public class Sesnsor : Grain, ISensor {
    private List<double> _measures = new List<double>();
    public async Task AddMeasure(double value) => _measures.Add(value);

    public async Task<double> GetAverage() => _measures.Average();
  }

  public class Counter : Grain, ICounter
  {
    private int _count = 0;
    public async Task<int> GetCount() => _count;

    public async Task Increment() => _count++;
  }

  [StorageProvider(ProviderName = "LocalAzureStorageEmulator")]
  public class PersistentCounter : Grain<int>, IPersistentCounter   {
    public async Task<int> GetCount() {
      if (State % 2 == 0)
        throw new Exception("even");
      return State;
    }

    public async Task Increment() {
      //Console.WriteLine("Incr");
      State++;
      await WriteStateAsync();
    }
  }

  public class Queue : Grain, IQueue
  {
    public async Task ProcessAsync(TimeSpan duration)
    {
      Console.WriteLine("Processing ...");
      await Task.Delay(duration);
    }
  }

  public class DeactivationTest : Grain, IDeactivationTest
  {
    private int i = 0;
    public DeactivationTest()
    {
      Console.WriteLine("Constructor ...");
    }

    public override Task OnActivateAsync()
    {
      Console.WriteLine("Activated ...");
      return base.OnActivateAsync();
    }

    public override Task OnDeactivateAsync()
    {
      Console.WriteLine("Deactivated ...");
      return base.OnDeactivateAsync();
    }

    public async Task<int> GetCount() => i;

    public async Task Incr() => i++;
  }

  [Reentrant]
  public class ReentrantQueue : Grain, IReentrantQueue
  {
    public async Task ProcessAsync(TimeSpan duration)
    {
      Console.WriteLine("Processing ...");
      await Task.Delay(duration);
      Console.WriteLine("Done.");
    }
  }

  public class Caller : Grain, ICaller
  {
    public async Task CallAsync(ICallee callee)
    {
      Console.WriteLine("Calling ...");
      await callee.Called();
      Console.WriteLine("Called.");
    }
  }

  public class Callee : Grain, ICallee
  {
    public async Task Called()
    {
      Console.WriteLine("Inside callee");
    }
  }

  public class MaximalKeySize : Grain, IMaximalKeySize
  {
    public Task Foo()
    {
      return TaskDone.Done;
    }
  }
}
