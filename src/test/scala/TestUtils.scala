// See LICENSE for license details.

package mini

import chisel3._
import chisel3.util._
import chisel3.testers._
import scala.reflect.ClassTag
import scala.concurrent.{Future, Await, ExecutionContext}
import Instructions._

trait DatapathTest
object BypassTest extends DatapathTest {
  override def toString: String = "bypass test"
}
object ExceptionTest extends DatapathTest {
  override def toString: String = "exception test"
}
// Define your own test
object DfcTest extends DatapathTest{
  override def toString: String = "dfc test"
}

object InstGenTest extends DatapathTest{
  override def toString: String = "dfc InstGen test"
}


trait TestUtils {
  implicit def boolToBoolean(x: Bool) = x.litValue() == 1
  implicit def bitPatToUInt(b: BitPat) = BitPat.bitPatToUInt(b)
  implicit def uintToBitPat(u: UInt) = BitPat(u)
  implicit def bigIntToInt(x: BigInt) = x.toInt
  implicit def bigIntToBoolean(x: BigInt) = x != 0
  def toBigInt(x: Int) = (BigInt(x >>> 1) << 1) | (x & 0x1)

  def rs1(inst: UInt) = ((inst.litValue() >> 15) & 0x1f).toInt
  def rs2(inst: UInt) = ((inst.litValue() >> 20) & 0x1f).toInt
  def rd (inst: UInt) = ((inst.litValue() >> 7)  & 0x1f).toInt
  def csr(inst: UInt) =  (inst.litValue() >> 20)
  def reg(x: Int) = (x & ((1 << 5) - 1)).U(5.W)
  def imm(x: Int) = (x & ((1 << 20) - 1)).S(21.W)

  def Cat(l: Seq[Bits]): UInt = (l.tail foldLeft l.head.asUInt){(x, y) =>
    assert(x.isLit() && y.isLit())
    (x.litValue() << y.getWidth | y.litValue()).U((x.getWidth + y.getWidth).W)
  }
  def Cat(x: Bits, l: Bits*): UInt = Cat(x :: l.toList)

  //fence 指令，表示为32位bit
  val fence = Cat(0.U(4.W), 0xf.U(4.W), 0xf.U(4.W), 0.U(13.W), Opcode.MEMORY)
  //nop 表示为addi x0，x0，0
  val nop   = Cat(0.U(12.W), reg(0), Funct3.ADD, reg(0), Opcode.ITYPE)
  val csrRegs = CSR.regs map (_.litValue())

  private val csrMap  = (csrRegs zip List(
    "cycle", "time", "instret", "cycleh", "timeh", "instreth",
    "cyclew", "timew", "instretw", "cyclehw", "timehw", "instrethw",
    "mcpuid", "mimpid","mhartid", "mtvec", "mtdeleg", "mie",
    "mtimecmp", "mtime", "mtimeh", "mscratch", "mepc", "mcause", "mbadaddr", "mip",
    "mtohost", "mfromhost", "mstatus"
  )).toMap
  def csrName(csr: BigInt) = csrMap getOrElse (csr, csr.toString(16))

  private def inst_31(inst: UInt)    = ((inst.litValue() >> 31) & 0x1).U(1.W)
  private def inst_30_25(inst: UInt) = ((inst.litValue() >> 25) & 0x3f).U(6.W)
  private def inst_24_21(inst: UInt) = ((inst.litValue() >> 21) & 0xf).U(4.W)
  private def inst_20(inst: UInt)    = ((inst.litValue() >> 20) & 0x1).U(1.W)
  private def inst_19_12(inst: UInt) = ((inst.litValue() >> 12) & 0xff).U(8.W)
  private def inst_11_8(inst: UInt)  = ((inst.litValue() >> 8)  & 0xf).U(4.W)
  private def inst_7(inst: UInt)     = ((inst.litValue() >> 7)  & 0x1).U(1.W)

  def iimm(inst: UInt) = Cat(Cat(Seq.fill(21){inst_31(inst)}),
                             inst_30_25(inst), inst_24_21(inst), inst_20(inst))
  def simm(inst: UInt) = Cat(Cat(Seq.fill(21){inst_31(inst)}),
                             inst_30_25(inst), inst_11_8(inst), inst_7(inst))
  def bimm(inst: UInt) = Cat(Cat(Seq.fill(20){inst_31(inst)}),
                             inst_7(inst), inst_30_25(inst), inst_11_8(inst), 0.U(1.W))
  def uimm(inst: UInt) = Cat(inst_31(inst), inst_30_25(inst), inst_24_21(inst),
                             inst_20(inst), inst_19_12(inst), 0.U(12.W))
  def jimm(inst: UInt) = Cat(Cat(Seq.fill(12){inst_31(inst)}), inst_19_12(inst),
                             inst_20(inst), inst_30_25(inst), inst_24_21(inst), 0.U(1.W))
  def zimm(inst: UInt) = ((inst.litValue() >> 15) & 0x1f).U

  /* Define tests */
  val rnd = new scala.util.Random
  def rand_fn7 = (rnd.nextInt(1 << 7)).U(7.W)
  def rand_rs2 = (rnd.nextInt((1 << 5) - 1) + 1).U(5.W)
  def rand_rs1 = (rnd.nextInt((1 << 5) - 1) + 1).U(5.W)
  def rand_fn3 = (rnd.nextInt(1 << 3)).U(3.W) 
  def rand_rd  = (rnd.nextInt((1 << 5) - 1) + 1).U(5.W)
  def rand_csr = csrRegs(rnd.nextInt(csrRegs.size-1)).U
  def rand_inst = toBigInt(rnd.nextInt()).U
  def rand_addr = toBigInt(rnd.nextInt()).U
  def rand_data = toBigInt(rnd.nextInt()).U
  val zero_rd = 0.U(5.W)


  val insts: Seq[UInt]  = Seq(
    Cat(rand_fn7, rand_rs2, rand_rs1, rand_fn3, rand_rd, Opcode.LUI),
    Cat(rand_fn7, rand_rs2, rand_rs1, rand_fn3, rand_rd, Opcode.AUIPC), 
    Cat(rand_fn7, rand_rs2, rand_rs1, rand_fn3, rand_rd, Opcode.JAL),
    Cat(rand_fn7, rand_rs2, rand_rs1, 0.U(3.W), rand_rd, Opcode.JALR),
    Cat(rand_fn7, rand_rs2, rand_rs1, Funct3.BEQ, rand_rd, Opcode.BRANCH),
    Cat(rand_fn7, rand_rs2, rand_rs1, Funct3.BNE, rand_rd, Opcode.BRANCH),
    Cat(rand_fn7, rand_rs2, rand_rs1, Funct3.BLT, rand_rd, Opcode.BRANCH),
    Cat(rand_fn7, rand_rs2, rand_rs1, Funct3.BGE, rand_rd, Opcode.BRANCH),
    Cat(rand_fn7, rand_rs2, rand_rs1, Funct3.BLTU, rand_rd, Opcode.BRANCH),
    Cat(rand_fn7, rand_rs2, rand_rs1, Funct3.BGEU, rand_rd, Opcode.BRANCH),
    Cat(rand_fn7, rand_rs2, rand_rs1, Funct3.LB, rand_rd, Opcode.LOAD),
    Cat(rand_fn7, rand_rs2, rand_rs1, Funct3.LH, rand_rd, Opcode.LOAD),
    Cat(rand_fn7, rand_rs2, rand_rs1, Funct3.LW, rand_rd, Opcode.LOAD),
    Cat(rand_fn7, rand_rs2, rand_rs1, Funct3.LBU, rand_rd, Opcode.LOAD),
    Cat(rand_fn7, rand_rs2, rand_rs1, Funct3.LHU, rand_rd, Opcode.LOAD),
    Cat(rand_fn7, rand_rs2, rand_rs1, Funct3.SB, rand_rd, Opcode.STORE),
    Cat(rand_fn7, rand_rs2, rand_rs1, Funct3.SH, rand_rd, Opcode.STORE),
    Cat(rand_fn7, rand_rs2, rand_rs1, Funct3.SW, rand_rd, Opcode.STORE),
    Cat(rand_fn7, rand_rs2, rand_rs1, Funct3.ADD, rand_rd, Opcode.ITYPE),
    Cat(rand_fn7, rand_rs2, rand_rs1, Funct3.SLT, rand_rd, Opcode.ITYPE),
    Cat(rand_fn7, rand_rs2, rand_rs1, Funct3.SLTU, rand_rd, Opcode.ITYPE),
    Cat(rand_fn7, rand_rs2, rand_rs1, Funct3.XOR, rand_rd, Opcode.ITYPE),
    Cat(rand_fn7, rand_rs2, rand_rs1, Funct3.OR, rand_rd, Opcode.ITYPE),
    Cat(rand_fn7, rand_rs2, rand_rs1, Funct3.AND, rand_rd, Opcode.ITYPE),
    Cat(Funct7.U, rand_rs2, rand_rs1, Funct3.SLL, rand_rd, Opcode.ITYPE),
    Cat(Funct7.U, rand_rs2, rand_rs1, Funct3.SR, rand_rd, Opcode.ITYPE),
    Cat(Funct7.S, rand_rs2, rand_rs1, Funct3.SR, rand_rd, Opcode.ITYPE),
    Cat(Funct7.U, rand_rs2, rand_rs1, Funct3.ADD, rand_rd, Opcode.RTYPE),
    Cat(Funct7.S, rand_rs2, rand_rs1, Funct3.ADD, rand_rd, Opcode.RTYPE),
    Cat(Funct7.U, rand_rs2, rand_rs1, Funct3.SLL, rand_rd, Opcode.RTYPE),
    Cat(Funct7.U, rand_rs2, rand_rs1, Funct3.SLT, rand_rd, Opcode.RTYPE),
    Cat(Funct7.U, rand_rs2, rand_rs1, Funct3.SLTU, rand_rd, Opcode.RTYPE),
    Cat(Funct7.U, rand_rs2, rand_rs1, Funct3.XOR, rand_rd, Opcode.RTYPE),
    Cat(Funct7.U, rand_rs2, rand_rs1, Funct3.SR, rand_rd, Opcode.RTYPE),
    Cat(Funct7.S, rand_rs2, rand_rs1, Funct3.SR, rand_rd, Opcode.RTYPE),
    Cat(Funct7.U, rand_rs2, rand_rs1, Funct3.OR, rand_rd, Opcode.RTYPE),
    Cat(Funct7.U, rand_rs2, rand_rs1, Funct3.AND, rand_rd, Opcode.RTYPE),
    fence, FENCEI,
    Cat(rand_csr, rand_rs1, Funct3.CSRRW, rand_rd, Opcode.SYSTEM),
    Cat(rand_csr, rand_rs1, Funct3.CSRRS, rand_rd, Opcode.SYSTEM),
    Cat(rand_csr, rand_rs1, Funct3.CSRRC, rand_rd, Opcode.SYSTEM),
    Cat(rand_csr, rand_rs1, Funct3.CSRRWI, rand_rd, Opcode.SYSTEM),
    Cat(rand_csr, rand_rs1, Funct3.CSRRSI, rand_rd, Opcode.SYSTEM),
    Cat(rand_csr, rand_rs1, Funct3.CSRRCI, rand_rd, Opcode.SYSTEM),
    ECALL, EBREAK, ERET, nop, rand_inst,
    Cat(Funct7.DFC, rand_rs2, rand_rs1, Funct3.TWA, zero_rd, Opcode.DFC),
    Cat(Funct7.DFC, rand_rs2, rand_rs1, Funct3.TWD_AD, zero_rd, Opcode.DFC),
    Cat(Funct7.DFC, rand_rs2, rand_rs1, Funct3.TWD_IF, zero_rd, Opcode.DFC)
  )

  def RU(funct3: UInt, rd: Int, rs1: Int, rs2: Int) = 
    Cat(Funct7.U, reg(rs2), reg(rs1), funct3, reg(rd), Opcode.RTYPE)
  def RS(funct3: UInt, rd: Int, rs1: Int, rs2: Int) = 
    Cat(Funct7.S, reg(rs2), reg(rs1), funct3, reg(rd), Opcode.RTYPE)
  def I(funct3: UInt, rd: Int, rs1: Int, i: Int) = 
    Cat(imm(i)(11, 0), reg(rs1), funct3, reg(rd), Opcode.ITYPE)
  def L(funct3: UInt, rd: Int, rs1: Int, i: Int) = 
    Cat(imm(i)(11, 0), reg(rs1), funct3, reg(rd), Opcode.LOAD)
  def S(funct3: UInt, rs2: Int, rs1: Int, i: Int) =
    Cat(imm(i)(11, 5), reg(rs2), reg(rs1), funct3, imm(i)(4, 0), Opcode.STORE)
  def B(funct3: UInt, rs1: Int, rs2: Int, i: Int) =
    Cat(imm(i)(12), imm(i)(10, 5), reg(rs2), reg(rs1), funct3, imm(i)(4, 1), imm(i)(11), Opcode.BRANCH)
  def U(op: UInt, rd: Int, i: Int) = 
    Cat(imm(i)(20, 0), reg(rd), op)
  def J(rd: Int, i: Int) = 
    Cat(imm(i)(20), imm(i)(10, 1), imm(i)(11), imm(i)(19, 12), reg(rd), Opcode.JAL)
  def JR(rd: Int, rs1: Int, i: Int) = 
    Cat(imm(i)(11, 0), reg(rs1), 0.U(3.W), reg(rd), Opcode.JALR)
  def SYS(funct3: UInt, rd: Int, csr: UInt, rs1: Int) = 
    Cat(csr, reg(rs1), funct3, reg(rd), Opcode.SYSTEM)
  def DFCTW(funct3: UInt, rs1: Int, rs2: Int) =
    Cat(Funct7.DFC, reg(rs2), reg(rs1), funct3, zero_rd, Opcode.DFC)
//  def TWD_AD(funct3: UInt, rs1: Int, rs2: Int) =
//    Cat(Funct7.DFC, reg(rs2), reg(rs1), funct3, zero_rd, Opcode.DFC)
//  def TWD_IF(funct3: UInt, rs1: Int, rs2: Int) =
//    Cat(Funct7.DFC, reg(rs2), reg(rs1), funct3, zero_rd, Opcode.DFC)

  val fin = Cat(CSR.mtohost, reg(31), Funct3.CSRRW, reg(0), Opcode.SYSTEM)
  val bypassTest = List(
    I(Funct3.ADD, 1, 0, 1),  // ADDI x1, x0, 1   # x1 <- 1
    S(Funct3.SW, 1, 0, 12),  // SW   x1, x0, 12  # Mem[12] <- 1
    L(Funct3.LW, 2, 0, 12),  // LW   x2, x0, 12  # x2 <- 1
    RU(Funct3.ADD, 3, 2, 2), // ADD  x3, x2, x2  # x3 <- 2
    RS(Funct3.ADD, 4, 3, 2), // SUB  x4, x2, x3  # x4 <- 1
    RU(Funct3.SLL, 5, 3, 4), // SLL  x5, x2, x4  # x5 <- 4
    RU(Funct3.SLT, 6, 4, 5), // SLT  x6, x4, x5  # x6 <- 1
    B(Funct3.BEQ, 1, 6, 8),  // BEQ  x1, x6, 8   # go to the BGE branch
    J(0, 12),                // JAL  x0, 12      # skip nop
    B(Funct3.BGE, 4, 1, -4), // BGE  x4, x1, -4  # go to the jump
    nop, nop,
    RU(Funct3.ADD, 26,  0, 1), // ADD x26,  x0, x1  # x26 <- 1
    RU(Funct3.ADD, 27, 26, 2), // ADD x27, x26, x2  # x27 <- 2
    RU(Funct3.ADD, 28, 27, 3), // ADD x28, x27, x3  # x28 <- 4
    RU(Funct3.ADD, 29, 28, 4), // ADD x29, x28, x4  # x29 <- 5
    RU(Funct3.ADD, 30, 29, 5), // ADD x30, x29, x5  # x30 <- 9
    RU(Funct3.ADD, 31, 30, 6), // ADD x31, x31, x6  # x31 <- 10
    fin
  )
  val exceptionTest = List(
    fence,
    I(Funct3.ADD, 31, 0,  2),  // ADDI x31, x0,  1 # x31 <- 2
    I(Funct3.ADD, 31, 31, 1),  // ADDI x31, x31, 1 # x31 <- 3
    I(Funct3.ADD, 31, 31, 1),  // ADDI x31, x31, 1 # x31 <- 4
    0.U,                       // exception
    I(Funct3.ADD, 31, 31, 1),  // ADDI x31, x31, 1 # x31 <- 5
    I(Funct3.ADD, 31, 31, 1),  // ADDI x31, x31, 1 # x31 <- 6
    I(Funct3.ADD, 31, 31, 1),  // ADDI x31, x31, 1 # x31 <- 7
    fin
  )
  val dfcTest = List(
    I(Funct3.ADD, 31, 0, 1),     // ADDI x31, x0, 1  # x31 <- 0x1  For Verify...

    //write TableA Data = 0x03080010 in reg(5)
    U(Opcode.LUI, 5, 12416),     // LUI x5, 0x03080  # x5 <- 0x03080000
    I(Funct3.ADD, 5,  5,  16),   // ADDI x5, x5, 16  # x5 <- 0x03080000 + 0x010

    //write TableA Addr = 0x12 in reg(6)
    I(Funct3.ADD, 6, 0,  12),    // ADDI x6, x0, 9   # x6 <- 12
    DFCTW(Funct3.TWA, 5, 6),     // TWA x5, x6       # rs1 = wData, rs2 = waddr
    nop,

    //write TableD 1
    //write TableD_addr Data = 0x46403308 in reg(7)
    U(Opcode.LUI, 7, 287747),    // LUI x7, 0x46403  # x7 <- 0x46403000
    I(Funct3.ADD, 7, 7, 776),    // ADDI x7, x7, 16  # x7 <- 0x46403000 + 0x308

    //write TableD_addr Addr = 0x8 in reg(8)
    I(Funct3.ADD, 8, 0, 8),      // ADDI, x8, x0, 8  # x8 <- 0x8
    DFCTW(Funct3.TWD_AD, 7, 8),  // TWD_AD x7, x8    # rs1 = wData, rs2 = waddr
    nop,

    //write TableD_info Data = 0x0000024C in reg(9)
    U(Opcode.LUI, 9, 0),         // LUI x9, 0x0      # x9 <- 0x0
    I(Funct3.ADD, 9, 9, 588),   // ADDI x9, x9,588 # x9 <- 0x00000000 + 0x24C

    //write TableD_info Addr = 0x8 in reg(10)
    I(Funct3.ADD, 10, 0, 8),     // ADDI x10, x0, 8  # x10 <- 0x8
    DFCTW(Funct3.TWD_IF, 9, 10), // TWD_IF x9, x10 # rs1 = wData, rs2 = waddr
    nop,

    //wirte TableD 2
    //write TableD_addr Data = 0x46403328 in reg(7)
    U(Opcode.LUI, 7, 287747),    // LUI x7, 0x46403  # x7 <- 0x46403000
    I(Funct3.ADD, 7, 7, 808),    // ADDI x7, x7, 16  # x7 <- 0x46403000 + 0x328

    //write TableD_addr Addr = 0x9 in reg(8)
    I(Funct3.ADD, 8, 0, 9),      // ADDI, x8, x0, 8  # x8 <- 0x9
    DFCTW(Funct3.TWD_AD, 7, 8),  // TWD_AD x7, x8    # rs1 = wData, rs2 = waddr
    nop,

    //write TableD_info Data = 0x0000028C in reg(9)
    U(Opcode.LUI, 9, 0),         // LUI x9, 0x0      # x9 <- 0x0
    I(Funct3.ADD, 9, 9, 652),   // ADDI x9, x9, 652 # x9 <- 0x00000000 + 0x28C

    //write TableD_info Addr = 0x9 in reg(10)
    I(Funct3.ADD, 10, 0, 9),     // ADDI x10, x0, 8  # x10 <- 0x8
    DFCTW(Funct3.TWD_IF, 9, 10), // TWD_IF x9, x10 # rs1 = wData, rs2 = waddr
    nop,

    //write TableD 3
    //write TableD_addr Data = 0x46403348 in reg(7)
    U(Opcode.LUI, 7, 287747),    // LUI x7, 0x46403  # x7 <- 0x46403000
    I(Funct3.ADD, 7, 7, 840),    // ADDI x7, x7, 16  # x7 <- 0x46403000 + 0x348

    //write TableD_addr Addr = 0x10 in reg(8)
    I(Funct3.ADD, 8, 0, 10),      // ADDI, x8, x0, 8  # x8 <- 0x10
    DFCTW(Funct3.TWD_AD, 7, 8),  // TWD_AD x7, x8    # rs1 = wData, rs2 = waddr
    nop,

    //write TableD_info Data = 0x0000000C in reg(9)
    U(Opcode.LUI, 9, 0),         // LUI x9, 0x0      # x9 <- 0x0
    I(Funct3.ADD, 9, 9, 12),   // ADDI x9, x9, 12 # x9 <- 0x00000000 + 0x00C

    //write TableD_info Addr = 0x10 in reg(10)
    I(Funct3.ADD, 10, 0, 10),     // ADDI x10, x0, 8  # x10 <- 0x8
    DFCTW(Funct3.TWD_IF, 9, 10), // TWD_IF x9, x10 # rs1 = wData, rs2 = waddr
    nop,

    //memory access, using store inst, target addr = src + offset
    U(Opcode.LUI, 11, 287747),  // LUI x11, 0x46403  # x11 <- 0x46403000
    I(Funct3.ADD, 12, 0, 100),  // ADDI x1, x0, 1   # x1 <- 1
    S(Funct3.SW, 12, 11, 776),     // SW   x12, x11, 776  # Mem[0x46403308] <- 100
    S(Funct3.SW, 12, 11, 808),     // SW   x12, x11, 808  # Mem[0x46403328] <- 100
    S(Funct3.SW, 12, 11, 840),     // SW   x12, x11, 12  # Mem[0x46403348] <- 100
    nop, nop,
    
    I(Funct3.ADD, 31, 0, 9),     // ADDI x31, x0, 9  # x31 <- 0x9
    fin
  )
  val instGenTest = List(
    // mv t0, a0  # t0 = x5 as waddr
    // mv t1, a1  # t1 = x6 as wData

    DFCTW(Funct3.TWA, 6, 5), // TWA x6, x5     # rs1 = wData, rs2 = waddr
    DFCTW(Funct3.TWD_AD, 6, 5), // TWD_AD x6, x5     # rs1 = wData, rs2 = waddr
    DFCTW(Funct3.TWD_IF, 6, 5), // TWA_IF x6, x5     # rs1 = wData, rs2 = waddr
    
    I(Funct3.ADD, 31, 0, 9), // ADDI x31, x0, 9  # x31 <- 0x9
    fin
  )

  val tests = Map(
    BypassTest    -> bypassTest,
    ExceptionTest -> exceptionTest,
    DfcTest -> dfcTest,
    InstGenTest -> instGenTest)
  val testResults = Map(
    BypassTest    -> 10,
    ExceptionTest -> 4,
    DfcTest -> 1,
    InstGenTest -> 9// For Finished Verify...
  )
}

trait HexUtils {
  def parseNibble(hex: Int) = if (hex >= 'a') hex - 'a' + 10 else hex - '0'
  // Group 256 chunks together
  // because big vecs dramatically increase compile time... :(
  def loadMem(lines: Iterator[String], chunk: Int) = ((lines flatMap { line =>
    assert(line.length % (chunk / 4) == 0)
    ((line.length - (chunk / 4)) to 0 by -(chunk / 4)) map { i =>
      ((0 until (chunk / 4)) foldLeft BigInt(0)){ (inst, j) =>
        inst | (BigInt(parseNibble(line(i + j))) << (4 * ((chunk / 4) - (j + 1))))
      }
    }
  }) map (_.U(chunk.W)) sliding (1 << 8, 1 << 8)).toSeq
}

object TestParams {
  implicit val p = 
    (new MiniConfig).toInstance alterPartial { case Trace => false }
}

abstract class IntegrationTests[T <: BasicTester : ClassTag](
    tester: (Iterator[String], Long) => T,
    testType: TestType,
    N: Int = 6) extends org.scalatest.FlatSpec {
  val dutName = implicitly[ClassTag[T]].runtimeClass.getSimpleName
  behavior of dutName
  import scala.concurrent.duration._
  import ExecutionContext.Implicits.global

  val results = testType.tests sliding (N, N) map { subtests =>
    val subresults = subtests map { test =>
      val stream = getClass.getResourceAsStream(s"/$test.hex")
      val loadmem = io.Source.fromInputStream(stream).getLines
      Future(test -> (TesterDriver execute (() => tester(loadmem, testType.maxcycles))))
    }
    Await.result(Future.sequence(subresults), Duration.Inf)
  }
  results.flatten foreach { case (name, pass) => it should s"pass $name" in { assert(pass) } }
}
