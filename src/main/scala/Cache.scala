// See LICENSE for license details.

package mini

import chisel3._
import chisel3.util._
import junctions._
import freechips.rocketchip.config.{Parameters, Field}

case object NWays extends Field[Int]
case object NSets extends Field[Int]
case object CacheBlockBytes extends Field[Int]

class CacheReq(implicit p: Parameters) extends CoreBundle()(p) {
  val addr = UInt(xlen.W)
  val data = UInt(xlen.W)
  //掩码位4位位宽
  val mask = UInt((xlen/8).W)
}

//cache里的数据
class CacheResp(implicit p: Parameters) extends CoreBundle()(p) {
  val data = UInt(xlen.W)
}

//req 为输入的cache访问请求，cpu输出线相连
//abort ?
//resp 为cache访问数据
class CacheIO (implicit val p: Parameters) extends Bundle {
  val abort = Input(Bool())
  val req   = Flipped(Valid(new CacheReq))
  //resp 为cache里的数据
  //valid 包裹，一个表示有效无效的bool输出类型，一个为输出数据
  val resp  = Valid(new CacheResp)
}

// cache 端口集合
class CacheModuleIO(implicit val p: Parameters) extends Bundle {
  val cpu   = new CacheIO
  val nasti = new NastiIO
}

trait CacheParams extends CoreParams with HasNastiParameters {
  val nWays  = p(NWays) // Not used... 单路，给组相连扩展，没有被使用
  // 数据块4字大小，按照字节编址？
  // 256 nset，256个组
  val nSets  = p(NSets)
  //cache 块字节
  // 16B
  val bBytes = p(CacheBlockBytes)
  //cache 块bit=128bit
  val bBits  = bBytes << 3
  //blen=4
  val blen   = log2Ceil(bBytes)
  //slen=8
  val slen   = log2Ceil(nSets)

  // tag 长度, 从tag长度推映射方法，直接映射
  val tlen   = xlen - (slen + blen)
  // 4
  val nWords = bBits / xlen
  // 4byte
  val wBytes = xlen / 8
  // 偏移2
  val byteOffsetBits = log2Ceil(wBytes)
  //这个值是2
  val dataBeats = bBits / nastiXDataBits
}

class MetaData(implicit val p: Parameters) extends Bundle with CacheParams {
  //tag 位数据
  val tag   = UInt(tlen.W)
}

class Cache(implicit val p: Parameters) extends Module with CacheParams {
  import Chisel._
  // FIXME: read enable signals for memories are broken by new chisel
  val io = IO(new CacheModuleIO)
  // cache states = 7
  val (s_IDLE :: s_READ_CACHE :: s_WRITE_CACHE :: s_WRITE_BACK :: s_WRITE_ACK ::
       s_REFILL_READY :: s_REFILL :: Nil) = Enum(7)
  //cache状态寄存器，初始化为空闲
  val state = RegInit(s_IDLE)
  // memory
  // 256宽度的寄存器两个 ---- v标志是否有效 d标志是否为脏
  val v        = RegInit(0.U(nSets.W))
  val d        = RegInit(0.U(nSets.W))
  //mem 存储构成
  //256个tag mem，宽度为tag 20位宽度
  val metaMem  = SeqMem(nSets, new MetaData)

  //256个 SeqMem 被填充
  //Vec类型 第一个参数表示个数，第二个是位宽

  //Seq类型 按照索引读取，该位置放置4个存储行，每行256*32bit
  val dataMem  = Seq.fill(nWords)(SeqMem(nSets, Vec(wBytes, UInt(8.W))))

  val addr_reg = Reg(io.cpu.req.bits.addr.cloneType)
  val cpu_data = Reg(io.cpu.req.bits.data.cloneType)
  val cpu_mask = Reg(io.cpu.req.bits.mask.cloneType)

  // Counters
  require(dataBeats > 0)
  //scala自带实现的计数器
  val (read_count,  read_wrap_out)  = Counter(io.nasti.r.fire(), dataBeats)
  val (write_count, write_wrap_out) = Counter(io.nasti.w.fire(), dataBeats)

  //状态判定

  val is_idle   = state === s_IDLE
  val is_read   = state === s_READ_CACHE
  val is_write  = state === s_WRITE_CACHE
  val is_alloc  = state === s_REFILL && read_wrap_out
  val is_alloc_reg = RegNext(is_alloc)

  //命中，电路节点类型
  val hit = Wire(Bool())
  //写使能 可写且命中或可分配，且cpu不中断或可分配
  val wen = is_write && (hit || is_alloc_reg) && !io.cpu.abort || is_alloc
  //读使能 需要空闲且cpu可用
  val ren = !wen && (is_idle || is_read) && io.cpu.req.valid
  //延迟一个周期读寄存器
  val ren_reg = RegNext(ren)

  //访问地址
  val addr     = io.cpu.req.bits.addr
  //idx 11-4 位置共8bit
  val idx      = addr(slen+blen-1, blen)
  //tag 31-12
  val tag_reg  = addr_reg(xlen-1, slen+blen)
  val idx_reg  = addr_reg(slen+blen-1, blen)
  //addr_reg(3,2)
  val off_reg  = addr_reg(blen-1, byteOffsetBits)

  //直接读取，按照索引位置读
  val rmeta = metaMem.read(idx, ren)
  //拼接data cache内容，注意按照低地址cat
  val rdata = Cat((dataMem map (_.read(idx, ren).asUInt)).reverse)
  val rdata_buf = RegEnable(rdata, ren_reg)
  //2 nasti位宽64，cache块的大小，这个buf是缓冲，数量为2
  val refill_buf = Reg(Vec(dataBeats, UInt(nastiXDataBits.W)))
  //从refill_buf or rdata中取出数据
  val read = Mux(is_alloc_reg, refill_buf.asUInt, Mux(ren_reg, rdata, rdata_buf))
  //是否命中
  hit := v(idx_reg) && rmeta.tag === tag_reg 

  // Read Mux
  //按字节赋值到电路，外部电路请求的内容
  io.cpu.resp.bits.data := Vec.tabulate(nWords)(i => read((i+1)*xlen-1, i*xlen))(off_reg)
  //cache内容有效性
  io.cpu.resp.valid     := is_idle || is_read && hit || is_alloc_reg && !cpu_mask.orR
  //有效向cpu传递
  when(io.cpu.resp.valid) { 
    addr_reg  := addr
    cpu_data  := io.cpu.req.bits.data
    cpu_mask  := io.cpu.req.bits.mask
  }
  //写tag的线网
  val wmeta = Wire(new MetaData)
  wmeta.tag := tag_reg
  //zext 为0转换为SInt
  val wmask = Mux(!is_alloc, (cpu_mask << Cat(off_reg, 0.U(byteOffsetBits.W))).zext, SInt(-1))
  val wdata = Mux(!is_alloc, Fill(nWords, cpu_data), 
    if (refill_buf.size == 1) io.nasti.r.bits.data
    else Cat(io.nasti.r.bits.data, Cat(refill_buf.init.reverse)))
  when(wen) {
    v := v.bitSet(idx_reg, true.B)
    d := d.bitSet(idx_reg, !is_alloc)
    when(is_alloc) {
      metaMem.write(idx_reg, wmeta)
    }
    dataMem.zipWithIndex foreach { case (mem, i) =>
      val data = Vec.tabulate(wBytes)(k => wdata(i*xlen+(k+1)*8-1, i*xlen+k*8))
      mem.write(idx_reg, data, wmask((i+1)*wBytes-1, i*wBytes).toBools)

      mem suggestName s"dataMem_${i}"
    }
  }
  //汇报总线状态，读地址，id, tag+idx addr, size 3, len 1
  io.nasti.ar.bits := NastiReadAddressChannel(
    0.U, Cat(tag_reg, idx_reg) << blen.U, log2Up(nastiXDataBits/8).U, (dataBeats-1).U)
  io.nasti.ar.valid := false.B
  // read data
  io.nasti.r.ready := state === s_REFILL
  when(io.nasti.r.fire()) { refill_buf(read_count) := io.nasti.r.bits.data }

  // write addr
  io.nasti.aw.bits := NastiWriteAddressChannel(
    0.U, Cat(rmeta.tag, idx_reg) << blen.U, log2Up(nastiXDataBits/8).U, (dataBeats-1).U)
  io.nasti.aw.valid := false.B

  // write data
  io.nasti.w.bits := NastiWriteDataChannel(
    Vec.tabulate(dataBeats)(i => read((i+1)*nastiXDataBits-1, i*nastiXDataBits))(write_count),
    None, write_wrap_out)
  io.nasti.w.valid := false.B
  // write resp
  io.nasti.b.ready := false.B

  // Cache FSM
  val is_dirty = v(idx_reg) && d(idx_reg)
  switch(state) {

    is(s_IDLE) {
      //cpu req valid = true
      //mask 的每一位 或
      when(io.cpu.req.valid) {
        state := Mux(io.cpu.req.bits.mask.orR, s_WRITE_CACHE, s_READ_CACHE)
      }
    }
    is(s_READ_CACHE) {
      when(hit) {
        when(io.cpu.req.valid) {
          state := Mux(io.cpu.req.bits.mask.orR, s_WRITE_CACHE, s_READ_CACHE)
        }.otherwise {
          state := s_IDLE
        }
      }.otherwise {
        io.nasti.aw.valid := is_dirty 
        io.nasti.ar.valid := !is_dirty
        when(io.nasti.aw.fire()) {
          state := s_WRITE_BACK
        }.elsewhen(io.nasti.ar.fire()) {
          state := s_REFILL
        }
      }
    }
    is(s_WRITE_CACHE) {
      when(hit || is_alloc_reg || io.cpu.abort) {
        state := s_IDLE
      }.otherwise {
        io.nasti.aw.valid := is_dirty
        io.nasti.ar.valid := !is_dirty
        when(io.nasti.aw.fire()) {
          state := s_WRITE_BACK
        }.elsewhen(io.nasti.ar.fire()) {
          state := s_REFILL
        }
      }
    }
    is(s_WRITE_BACK) {
      io.nasti.w.valid := true.B
      when(write_wrap_out) {
        state := s_WRITE_ACK
      }
    }
    is(s_WRITE_ACK) {
      io.nasti.b.ready := true.B
      when(io.nasti.b.fire()) {
        state := s_REFILL_READY
      }
    }
    is(s_REFILL_READY) {
      io.nasti.ar.valid := true.B
      when(io.nasti.ar.fire()) {
        state := s_REFILL
      }
    }
    is(s_REFILL) {
      when(read_wrap_out) {
        state := Mux(cpu_mask.orR, s_WRITE_CACHE, s_IDLE) 
      }
    }
  }
}
