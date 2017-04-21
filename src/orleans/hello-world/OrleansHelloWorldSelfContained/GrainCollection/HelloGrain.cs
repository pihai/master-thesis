using GrainInterfaces;
using Orleans;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace GrainCollection
{
  public class HelloGrain : Grain, IHello
  {
    public Task<string> SayHello()
    {
      return Task.FromResult("Hello");
    }
  }
}
