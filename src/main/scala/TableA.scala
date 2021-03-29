
package mini

import chisel3._
import chisel3.util._
import junctions._
import freechips.rocketchip.config.{Field, Parameters}


class A_counterPart extends Module {
  //counter 部件
  val io = IO(new Bundle() {
    val operationAddr = Input(UInt(6.W))

    val dIn = Input(UInt(8.W))
    val load = Input(Bool())

    val countDownEn = Input(Bool())

    val interruptSignal = Output(Bool())
    val equalZeroAddr = Output(UInt(6.W))
  })

  //internal Meta
  val counterMeta = Mem(64, UInt(8.W))
  //valid Reg
  val valid = RegInit(0.U(64.W))

  // TODO: cancle buffered input regs
  val lastoperationAddr = RegInit(0.U)
  lastoperationAddr := io.operationAddr
  val lastcountDownEn = RegInit(false.B)
  lastcountDownEn := io.countDownEn
  val lastload = RegInit(false.B)
  lastload := io.load
  val lastdIn = RegInit(0.U)
  lastdIn := io.dIn
  val equalZeroAddr = RegInit(0.U)

  //next init
  val next = WireInit(0.U)
  //current Count
  val currentCount = Wire(UInt())

  val stimulate = RegInit(false.B)

  val equalZero = WireInit(false.B)

  when(lastload && !valid(lastoperationAddr)){
    //load condition
    printf("[WTA] counterPart(id: %d) = %d\n", lastoperationAddr, lastdIn)
    counterMeta(lastoperationAddr) := lastdIn
    valid := valid.bitSet(lastoperationAddr, true.B)
  }.elsewhen(lastload && valid(lastoperationAddr)){
    printf("[FATAL] Failed signals, load = %d, valid = %d\n", lastload, valid(lastoperationAddr))
  }

  when(lastcountDownEn && valid(lastoperationAddr)){
    counterMeta.write(lastoperationAddr, next)
  }.elsewhen(lastcountDownEn && !valid(lastoperationAddr)){
    printf("[FATAL] Failed signals, countDownEn = %d, valid = %d\n", lastcountDownEn, valid(lastoperationAddr))
  }

  //read from mem with operationAddr
  currentCount := counterMeta.read(lastoperationAddr)

  //countDownEn arrive judge next time = curreentCount - 1
  when(lastcountDownEn && valid(lastoperationAddr)){
    equalZero := currentCount - 1.U === 0.U
  }.otherwise{
    equalZero := false.B
  }

  when(equalZero) {
    stimulate := true.B
    equalZeroAddr := lastoperationAddr
  }.otherwise{
    stimulate := false.B
  }

  //stimulate
  when(stimulate){
    stimulate := false.B
    valid := valid.bitSet(equalZeroAddr, false.B)
    counterMeta.write(equalZeroAddr, 255.U)
  }

  io.interruptSignal := stimulate
  io.equalZeroAddr := equalZeroAddr

  printf("[CNT]counterPart.stimulate = %d\n", stimulate)
}

//A table Meta
class A_Meta extends Bundle {
  val inputLink = UInt(8.W)
  val pId = UInt(16.W)
}

//Separated dfc_A IO imp
class dfc_AIO extends Bundle {
  val wEn = Input(Bool())
  val wData = Input(UInt(32.W))

  //addr wire merge
  val opAddr = Input(UInt(6.W))
  //val rAddr = Input(UInt(6.W))

  val counterDownEn = Input(Bool())
  val counterDownAddr = Input(UInt(6.W))

  val rData = Output(UInt(16.W))
  val interruptPost = Output(Bool())
  val equalZeroAddr = Output(UInt(6.W))
}

//Table A
class dfc_A(implicit val p: Parameters) extends Module with CoreParams {

  val io = IO(new dfc_AIO)

  // TODO: cancle buffered input regs
  val lastwEn = RegInit(false.B)
  lastwEn := io.wEn
  val lastwData = RegInit(0.U)
  lastwData := io.wData
  val lastopAddr = RegInit(0.U)
  lastopAddr := io.opAddr
  val lastcounterDownEn = RegInit(false.B)
  lastcounterDownEn := io.counterDownEn
  val lastcounterDownAddr = RegInit(0.U)
  lastcounterDownAddr := io.counterDownAddr

  //Outputs init, if not, compile will report not fully initialized error
  io.rData := 0.U
  io.interruptPost := false.B

  //64 Meta lines
  val Metamem = Mem(64, new A_Meta)
  val valid = RegInit(0.U(64.W))
  //A counter Part
  val counterPart = Module(new A_counterPart)

  val addr_wire = Wire(UInt(6.W))
  val data_wire = Wire(UInt(32.W))
  val counterPartInterrupt_wire = Wire(Bool())

  //counterPart IO connect
  counterPart.io.load := lastwEn //link with io.wEn
  counterPart.io.dIn := 0.U

  counterPart.io.countDownEn := lastcounterDownEn //link with io.counterDownEn

  //counterPart interruptSignal transfer
  counterPartInterrupt_wire := counterPart.io.interruptSignal

  //wirte addr & data wire
  //  addr_wire := io.opAddr
  //  data_wire := io.wData
  addr_wire := lastopAddr
  data_wire := lastwData

  //cut data to parts
  val wCount = data_wire(31,24)
  val winputLink = data_wire(23,16)
  val wpId = data_wire(15,0)

  //write Meta info init, if not, report not fully initialzled error
  val wMeta = Wire(new A_Meta)
  wMeta.inputLink := winputLink
  wMeta.pId := wpId

  //write A table
  when(lastwEn && !valid(lastopAddr)) {
    //write Meta
    printf("[WTA](id: %d) inputLink = %d, pId = %d, count = %d\n", lastopAddr, winputLink, wpId, wCount)
    Metamem.write(addr_wire, wMeta)

    //write Counterneed load, dIn
    counterPart.io.load := true.B
    counterPart.io.dIn := wCount

    //valid bit
    valid := valid.bitSet(addr_wire, true.B)
  }

  //counterPart addr select
  when(lastcounterDownEn && !lastwEn){
    counterPart.io.operationAddr := lastcounterDownAddr
    //counterPart.io.countDownEn := true.B
  }.elsewhen(!lastcounterDownEn && lastwEn){
    counterPart.io.operationAddr := lastopAddr
    //counterPart.io.countDownEn := false.B
  }.otherwise{
    counterPart.io.operationAddr := lastopAddr
  }

  //sync with counterPart.interruptSignal
  when(counterPartInterrupt_wire) {
    printf("[POST]One line of TableA had been set ZERO, Interrupt!\n")
    io.interruptPost := true.B
    // when Interrupt post, DFC transfer TableA pid info to outside
    io.rData := Metamem(counterPart.io.equalZeroAddr).pId
    // set line valid to false
    valid := valid.bitSet(counterPart.io.equalZeroAddr, false.B)
  }

  //uncovered port
  io.equalZeroAddr := counterPart.io.equalZeroAddr

  if(p(Trace)){
    //printf("Metamem(%d).inputLink = %d\n", io.opAddr, Metamem.read(io.opAddr).inputLink)
    //printf("Metamem(%d).pId = %d\n", io.opAddr, Metamem.read(io.opAddr).pId)
    printf("[RTA]io.rData = %d\n", io.rData)
  }
}







