using GrainInterfaces;
using Orleans;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace GrainsCollection
{
  public class Hello : Grain, IHello {
    public Task<string> SayHello() {
      this.GetPrimaryKey();
      return Task.FromResult("hello " + this.IdentityString + " " + this.RuntimeIdentity);
    }
  }
}
