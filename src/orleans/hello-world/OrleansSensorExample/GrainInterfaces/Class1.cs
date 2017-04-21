using Orleans;
using Orleans.Concurrency;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace GrainInterfaces
{
  public interface ISensor : IGrainWithGuidKey
  {
    Task AddMeasure(double value);
    Task<double> GetAverage();
  }

  public interface ICounter : IGrainWithIntegerKey
  {
    Task Increment();
    Task<int> GetCount();
  }

  public interface IPersistentCounter : IGrainWithIntegerKey
  {
    Task Increment();
    Task<int> GetCount();
  }

  public interface IQueue : IGrainWithIntegerKey
  {
    Task ProcessAsync(TimeSpan duration);
  }

  public interface IDeactivationTest : IGrainWithIntegerKey
  {
    Task Incr();
    Task<int> GetCount();
  }

  public interface IReentrantQueue : IGrainWithIntegerKey
  {
    Task ProcessAsync(TimeSpan duration);
  }

  public interface ICaller : IGrainWithIntegerKey
  {
    Task CallAsync(ICallee callee);
  }

  public interface ICallee : IGrainWithIntegerKey
  {
    Task Called();
  }

  public interface IMaximalKeySize : IGrainWithStringKey
  {
    Task Foo();
  }
}
