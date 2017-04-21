using System.Threading.Tasks;
using Orleans;

namespace OrleansHelloWorld.GrainInterfaces
{
  /// <summary>
  /// Grain interface IGrain1
  /// </summary>
  public interface IGrain1 : IGrainWithGuidKey
  {
    Task<string> SayHello();
  }
}
