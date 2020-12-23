// See LICENSE for license details.

package mini

import chisel3.Module
import freechips.rocketchip.config.{Parameters, Config}
import junctions._

class MiniConfig extends Config((site, here, up) => {
    // Core
    case XLEN => 32
    case Trace => true
    case BuildALU    => (p: Parameters) => Module(new ALUArea()(p))
    case BuildImmGen => (p: Parameters) => Module(new ImmGenWire()(p))
    case BuildBrCond => (p: Parameters) => Module(new BrCondArea()(p))
    // Cache 单路
    case NWays => 1 // TODO: set-associative 组相连
    case NSets => 256 //cache
    //case DFCSets_N => 256 //DFC M > k*N k为平均输入个数
    //case DFCSets_M => 256 //DFC
    case CacheBlockBytes => 4 * (here(XLEN) >> 3) // 4 x 32 bits = 16B 字节单位
    // NastiIO
    case NastiKey => new NastiParameters(
      idBits   = 5,
      dataBits = 64,
      addrBits = here(XLEN))
  }
)
