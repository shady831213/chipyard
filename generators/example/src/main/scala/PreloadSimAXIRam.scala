package example

import chipsalliance.rocketchip.config.{Config, Field, Parameters}
import chisel3._
import chisel3.experimental.{ChiselAnnotation, annotate}
import chisel3.internal.InstanceId
import chisel3.util._
import firrtl.annotations.Annotation
import freechips.rocketchip.amba.axi4.{AXI4Buffer, AXI4EdgeParameters, AXI4Fragmenter, AXI4MasterNode, AXI4Parameters, AXI4RAM, AXI4SlaveNode, AXI4SlaveParameters, AXI4SlavePortParameters, AXI4Xbar}
import freechips.rocketchip.diplomacy.{AddressSet, DiplomaticSRAM, InModuleBody, LazyModule, LazyModuleImp, RegionType, SimpleLazyModule, TransferSizes}
import freechips.rocketchip.diplomaticobjectmodel.DiplomaticObjectModelAddressing
import freechips.rocketchip.diplomaticobjectmodel.logicaltree.{BusMemoryLogicalTreeNode, LogicalModuleTree, LogicalTreeNode}
import freechips.rocketchip.diplomaticobjectmodel.model.{AXI4_Lite, OMMemory, OMRTLModule, OMSRAM}
import freechips.rocketchip.subsystem.{CanHaveMasterAXI4MemPort, ExtMem}
import freechips.rocketchip.util._
import chisel3.util.experimental._

case object MemPreloadFile extends Field[Option[String]](None)

class WithMemPreloadFile extends Config((site, here, up) => {
  case MemPreloadFile => Some(s"./preload_ftsi.hex")
})

/** Memory with AXI port for use in elaboratable test harnesses. */
class PreloadSimAXIMem(edge: AXI4EdgeParameters, size: BigInt)(implicit p: Parameters) extends SimpleLazyModule {
  private val hexfile = p(MemPreloadFile)
  val node = AXI4MasterNode(List(edge.master))
  val xbar = AXI4Xbar()
  hexfile match {
    case Some(name) => {
      val aSets = AddressSet.misaligned(0, size)
      require(aSets.length == 1, "preload memory dose not support splited memory!")
      val srams = aSets.map { aSet => LazyModule(new PreloadAXI4RAM(aSet, name, beatBytes = edge.bundle.dataBits / 8)) }
      srams.foreach { s => s.node := AXI4Buffer() := AXI4Fragmenter() := xbar }
    }
    case None => {
      require(false, "unreachable!")
    }
  }
  xbar := node
  val io_axi4 = InModuleBody {
    node.makeIOs()
  }
}

object PreloadSimAXIMem {
  def connectMem(dut: CanHaveMasterAXI4MemPort)(implicit p: Parameters): Seq[PreloadSimAXIMem] = {
    dut.mem_axi4.zip(dut.memAXI4Node.in).map { case (io, (_, edge)) =>
      val mem = LazyModule(new PreloadSimAXIMem(edge, size = p(ExtMem).get.master.size))
      Module(mem.module).suggestName("mem")
      mem.io_axi4.head <> io
      mem
    }
  }
}

class PreloadAXI4RAM(
                      address: AddressSet,
                      hexfile: String,
                      cacheable: Boolean = true,
                      parentLogicalTreeNode: Option[LogicalTreeNode] = None,
                      executable: Boolean = true,
                      beatBytes: Int = 4,
                      devName: Option[String] = None,
                      errors: Seq[AddressSet] = Nil,
                      wcorrupt: Boolean = false)
                    (implicit p: Parameters) extends DiplomaticSRAM(address, beatBytes, devName) {
  val node = AXI4SlaveNode(Seq(AXI4SlavePortParameters(
    Seq(AXI4SlaveParameters(
      address = List(address) ++ errors,
      resources = resources,
      regionType = if (cacheable) RegionType.UNCACHED else RegionType.IDEMPOTENT,
      executable = executable,
      supportsRead = TransferSizes(1, beatBytes),
      supportsWrite = TransferSizes(1, beatBytes),
      interleavedId = Some(0))),
    beatBytes = beatBytes,
    wcorrupt = wcorrupt,
    minLatency = 1)))

  def makeSinglePortedByteWriteMem(size: BigInt, lanes: Int = beatBytes, bits: Int = 8) = {
    // We require the address range to include an entire beat (for the write mask)

    val (mem, omSRAM) = PreloadDescribedSRAM(
      name = devName.getOrElse("mem"),
      desc = devName.getOrElse("mem"),
      size = size,
      data = Vec(lanes, UInt(bits.W)),
      hexfile = hexfile
    )
    devName.foreach(n => mem.suggestName(n.split("-").last))

    val omMem: OMMemory = DiplomaticObjectModelAddressing.makeOMMemory(
      desc = "mem", //lim._2.name.map(n => n).getOrElse(lim._1.name),
      depth = size,
      data = Vec(lanes, UInt(bits.W))
    )

    (mem, omSRAM, Seq(omMem))
  }

  implicit class MemAsSeqMem[T <: Data](val x: Mem[T]) {
    def syncRead(addr: UInt, ren: Bool) = {
      val r_addr = Wire(UInt(addr.getWidth.W))
      r_addr := DontCare
      var port: Option[T] = None
      when(ren) {
        r_addr := addr
        port = Some(x.read(r_addr))
      }
      port.get
    }

    def readAndHold(addr: UInt, enable: Bool): T = syncRead(addr, enable) holdUnless RegNext(enable)
  }

  lazy val module = new LazyModuleImp(this) {
    val (in, _) = node.in(0)
    val (mem, omSRAM, omMem) = makeSinglePortedByteWriteMem(size = 1 << mask.filter(b => b).size)

    parentLogicalTreeNode.map {
      case parentLTN =>
        def sramLogicalTreeNode = new BusMemoryLogicalTreeNode(
          device = device,
          omSRAMs = Seq(omSRAM),
          busProtocol = new AXI4_Lite(None),
          dataECC = None,
          hasAtomics = None,
          busProtocolSpecification = None)

        LogicalModuleTree.add(parentLTN, sramLogicalTreeNode)
    }

    val corrupt = if (wcorrupt) Some(Mem(1 << mask.filter(b => b).size, UInt(2.W))) else None

    val r_addr = Cat((mask zip (in.ar.bits.addr >> log2Ceil(beatBytes)).asBools).filter(_._1).map(_._2).reverse)
    val w_addr = Cat((mask zip (in.aw.bits.addr >> log2Ceil(beatBytes)).asBools).filter(_._1).map(_._2).reverse)
    val r_sel0 = address.contains(in.ar.bits.addr)
    val w_sel0 = address.contains(in.aw.bits.addr)

    val w_full = RegInit(Bool(), false.B)
    val w_id = Reg(UInt())
    val w_user = Reg(UInt((1 max in.params.userBits).W))
    val r_sel1 = RegInit(r_sel0)
    val w_sel1 = RegInit(w_sel0)

    when(in.b.fire()) {
      w_full := false.B
    }
    when(in.aw.fire()) {
      w_full := true.B
    }

    when(in.aw.fire()) {
      w_id := in.aw.bits.id
      w_sel1 := w_sel0
      in.aw.bits.user.foreach {
        w_user := _
      }
    }
    val wdata = VecInit(Seq.tabulate(beatBytes) { i => in.w.bits.data(8 * (i + 1) - 1, 8 * i) })
    val preWdata = mem.read(w_addr).asTypeOf(wdata)
    when(in.aw.fire() && w_sel0) {
      mem.write(w_addr, VecInit((in.w.bits.strb.asBools zip (wdata zip preWdata)).map { case (m, (w, r)) => Mux(m, w, r) }).asUInt())
      //      mem.write(w_addr, wdata, in.w.bits.strb.asBools)
      corrupt.foreach {
        _.write(w_addr, in.w.bits.corrupt.get.asUInt)
      }
    }

    in.b.valid := w_full
    in.aw.ready := in.w.valid && (in.b.ready || !w_full)
    in.w.ready := in.aw.valid && (in.b.ready || !w_full)

    in.b.bits.id := w_id
    in.b.bits.resp := Mux(w_sel1, AXI4Parameters.RESP_OKAY, AXI4Parameters.RESP_DECERR)
    in.b.bits.user.foreach {
      _ := w_user
    }

    val r_full = RegInit(Bool(), false.B)
    val r_id = Reg(UInt())
    val r_user = Reg(UInt((1 max in.params.userBits).W))

    when(in.r.fire()) {
      r_full := false.B
    }
    when(in.ar.fire()) {
      r_full := true.B
    }

    when(in.ar.fire()) {
      r_id := in.ar.bits.id
      r_sel1 := r_sel0
      in.ar.bits.user.foreach {
        r_user := _
      }
    }

    val ren = in.ar.fire()
    val rdata = mem.readAndHold(r_addr, ren).asTypeOf(preWdata)
    val rcorrupt = corrupt.map(_.readAndHold(r_addr, ren)(0)).getOrElse(false.B)

    in.r.valid := r_full
    in.ar.ready := in.r.ready || !r_full

    in.r.bits.id := r_id
    in.r.bits.resp := Mux(r_sel1, Mux(rcorrupt, AXI4Parameters.RESP_SLVERR, AXI4Parameters.RESP_OKAY), AXI4Parameters.RESP_DECERR)
    in.r.bits.data := Cat(rdata.reverse)
    in.r.bits.user.foreach {
      _ := r_user
    }
    in.r.bits.last := true.B
  }
}

object PreloadAXI4RAM {
  def apply(
             address: AddressSet,
             hexfile: String,
             cacheable: Boolean = true,
             parentLogicalTreeNode: Option[LogicalTreeNode] = None,
             executable: Boolean = true,
             beatBytes: Int = 4,
             devName: Option[String] = None,
             errors: Seq[AddressSet] = Nil,
           )
           (implicit p: Parameters) = {
    val axi4ram = LazyModule(new PreloadAXI4RAM(
      address = address,
      hexfile = hexfile,
      cacheable = cacheable,
      executable = executable,
      beatBytes = beatBytes,
      devName = devName,
      errors = errors))
    axi4ram.node
  }
}


object PreloadDescribedSRAM {
  def apply[T <: Data](
                        name: String,
                        desc: String,
                        size: BigInt, // depth
                        data: T,
                        hexfile: String
                      ): (Mem[UInt], OMSRAM) = {

    val mem = Mem(size, UInt(data.getWidth.W))

    loadMemoryFromFile(mem, hexfile)

    mem.suggestName(name)

    val granWidth = data match {
      case v: Vec[_] => v.head.getWidth
      case d => d.getWidth
    }

    val uid = 0

    val omSRAM = DiplomaticObjectModelAddressing.makeOMSRAM(
      desc = desc,
      width = data.getWidth,
      depth = size,
      granWidth = granWidth,
      uid = uid,
      rtlModule = OMRTLModule(moduleName = name)
    )

    Annotated.srams(
      component = mem,
      name = name,
      address_width = log2Ceil(size),
      data_width = data.getWidth,
      depth = size,
      description = desc,
      write_mask_granularity = granWidth
    )

    (mem, omSRAM)
  }

  def AnnotateSrams(
                     component: InstanceId,
                     name: String,
                     address_width: Int,
                     data_width: Int,
                     depth: BigInt,
                     description: String,
                     write_mask_granularity: Int,
                     hexfile: String): Unit = {
    annotate(new ChiselAnnotation {
      def toFirrtl: Annotation = SRAMAnnotation(
        component.toNamed,
        address_width = address_width,
        name = name,
        data_width = data_width,
        depth = depth,
        description = description,
        write_mask_granularity = write_mask_granularity
      )
    })
  }
}
