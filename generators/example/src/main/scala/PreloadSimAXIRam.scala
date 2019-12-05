package example

import chipsalliance.rocketchip.config.{Field, Parameters}
import chisel3._
import chisel3.experimental.{ChiselAnnotation, annotate}
import chisel3.internal.InstanceId
import chisel3.util._
import firrtl.annotations.Annotation
import freechips.rocketchip.amba.axi4.{AXI4Buffer, AXI4EdgeParameters, AXI4Fragmenter, AXI4MasterNode, AXI4RAM, AXI4Xbar}
import freechips.rocketchip.diplomacy.{AddressSet, InModuleBody, LazyModule, SimpleLazyModule}
import freechips.rocketchip.diplomaticobjectmodel.DiplomaticObjectModelAddressing
import freechips.rocketchip.diplomaticobjectmodel.logicaltree.LogicalTreeNode
import freechips.rocketchip.diplomaticobjectmodel.model.{OMMemory, OMRTLModule, OMSRAM}
import freechips.rocketchip.subsystem.{CanHaveMasterAXI4MemPort, ExtMem}
import freechips.rocketchip.util.{Annotated, SRAMAnnotation}
import chisel3.util.experimental._

case object MemPrelaodFile extends Field[Option[String]](None)

/** Memory with AXI port for use in elaboratable test harnesses. */
class PreloadSimAXIMem(edge: AXI4EdgeParameters, size: BigInt)(implicit p: Parameters) extends SimpleLazyModule {
  private val hexfile = p(MemPrelaodFile)
  val node = AXI4MasterNode(List(edge.master))
  val srams = hexfile match {
    case Some(name) => {
      val aSets = AddressSet.misaligned(0, size)
      require(aSets.length == 1, "preload memory dose not support splited memory!")
      aSets.map { aSet => LazyModule(new PreloadAXI4RAM(aSet, name, beatBytes = edge.bundle.dataBits / 8)) }
    }
    case None => AddressSet.misaligned(0, size).map { aSet => LazyModule(new AXI4RAM(aSet, beatBytes = edge.bundle.dataBits / 8)) }
  }
  val xbar = AXI4Xbar()
  srams.foreach { s => s.node := AXI4Buffer() := AXI4Fragmenter() := xbar }
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
                    (implicit p: Parameters) extends AXI4RAM(address, cacheable, parentLogicalTreeNode, executable, beatBytes, devName, errors, wcorrupt) {
  override def makeSinglePortedByteWriteSeqMem(size: BigInt, lanes: Int = beatBytes, bits: Int = 8) = {
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
                      ): (SyncReadMem[T], OMSRAM) = {

    val mem = SyncReadMem(size, data)

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
