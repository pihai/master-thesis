using System;
using System.Threading.Tasks;
using Orleans;
using OrleansHelloWorld.GrainInterfaces;

namespace OrleansHelloWorld.GrainCollection
{
  /// <summary>
  /// Grain implementation class Grain1.
  /// </summary>
  public class Grain1 : Grain, IGrain1
  {
    public Task<string> SayHello()
    {
      return Task.FromResult("Hello there ...");
    }
  }
}
