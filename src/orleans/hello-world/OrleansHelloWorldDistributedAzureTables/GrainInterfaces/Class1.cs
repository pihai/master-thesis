using Orleans;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace GrainInterfaces
{
    public interface IHello : IGrainWithGuidKey
    {
    Task<string> SayHello();
    }
}
