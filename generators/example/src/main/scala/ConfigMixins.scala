package example

import chisel3._
import chisel3.util.{log2Up}

import freechips.rocketchip.config.{Field, Parameters, Config}
import freechips.rocketchip.subsystem.{RocketTilesKey, WithRoccExample, WithNMemoryChannels, WithNBigCores, WithRV32}
import freechips.rocketchip.diplomacy.{LazyModule, ValName}
import freechips.rocketchip.devices.tilelink.BootROMParams
import freechips.rocketchip.devices.debug.{Debug}
import freechips.rocketchip.tile.{XLen, BuildRoCC, TileKey, LazyRoCC}

import boom.common.{BoomTilesKey}

import testchipip._

import hwacha.{Hwacha}

import sifive.blocks.devices.gpio._

/**
 * TODO: Why do we need this?
 */
object ConfigValName {
  implicit val valName = ValName("TestHarness")
}
import ConfigValName._

// -----------------------
// Common Parameter Mixins
// -----------------------

class WithBootROM extends Config((site, here, up) => {
  case BootROMParams => BootROMParams(
    contentFileName = s"./bootrom/bootrom.rv${site(XLen)}.img")
})

class WithGPIO extends Config((site, here, up) => {
  case PeripheryGPIOKey => List(
    GPIOParams(address = 0x10012000, width = 4, includeIOF = false))
  case BuildTop => (clock: Clock, reset: Bool, p: Parameters, success: Bool) => {
    val top = up(BuildTop, site)(clock, reset, p, success)
    for (gpio <- top.gpio) {
      for (pin <- gpio.pins) {
        pin.i.ival := false.B
      }
    }
    top
  }
})

class WithNoGPIO extends Config((site, here, up) => {
  case PeripheryGPIOKey => Seq()
})

class WithTSI extends Config((site, here, up) => {
  case SerialKey => true
  case BuildTop => (clock: Clock, reset: Bool, p: Parameters, success: Bool) => {
    val top = up(BuildTop, site)(clock, reset, p, success)
    success := top.connectSimSerial()
    top
  }
})

class WithDTM extends Config((site, here, up) => {
  case BuildTop => (clock: Clock, reset: Bool, p: Parameters, success: Bool) => {
    val top = up(BuildTop, site)(clock, reset, p, success)
    top.reset := reset.asBool | top.debug.ndreset
    Debug.connectDebug(top.debug, clock, reset.asBool, success)(p)
    top
  }
})

class WithPWMTL extends Config((site, here, up) => {
  case PWMKey => Some(PWMType(use_axi4=false))
})

class WithPWMAXI4 extends Config((site, here, up) => {
  case PWMKey => Some(PWMType(use_axi4=true))
})

class WithGCD extends Config((site, here, up) => {
  case GCDKey => true
})

class WithBlockDeviceModel extends Config((site, here, up) => {
  case BuildTop => (clock: Clock, reset: Bool, p: Parameters, success: Bool) => {
    val top = up(BuildTop, site)(clock, reset, p, success)
    top.connectBlockDeviceModel()
    top
  }
})

class WithSimBlockDevice extends Config((site, here, up) => {
  case BuildTop => (clock: Clock, reset: Bool, p: Parameters, success: Bool) => {
    val top = up(BuildTop, site)(clock, reset, p, success)
    top.connectSimBlockDevice(clock, reset)
    top
  }
})

class WithInitZero(base: BigInt, size: BigInt) extends Config((site, here, up) => {
  case InitZeroKey => InitZeroConfig(base, size)
})

// ------------------
// Multi-RoCC Support
// ------------------

/**
 * Map from a hartId to a particular RoCC accelerator
 */
case object MultiRoCCKey extends Field[Map[Int, Seq[Parameters => LazyRoCC]]](Map.empty[Int, Seq[Parameters => LazyRoCC]])

/**
 * Mixin to enable different RoCCs based on the hartId
 */
class WithMultiRoCC extends Config((site, here, up) => {
  case BuildRoCC => site(MultiRoCCKey).getOrElse(site(TileKey).hartId, Nil)
})

/**
 * Mixin to add Hwachas to cores based on hart
 *
 * For ex:
 *   Core 0, 1, 2, 3 have been defined earlier
 *     with hartIds of 0, 1, 2, 3 respectively
 *   And you call WithMultiRoCCHwacha(0,1)
 *   Then Core 0 and 1 will get a Hwacha
 *
 * @param harts harts to specify which will get a Hwacha
 */
class WithMultiRoCCHwacha(harts: Int*) extends Config((site, here, up) => {
  case MultiRoCCKey => {
    require(harts.max <= ((up(RocketTilesKey, site).length + up(BoomTilesKey, site).length) - 1))
    up(MultiRoCCKey, site) ++ harts.distinct.map{ i =>
      (i -> Seq((p: Parameters) => {
        LazyModule(new Hwacha()(p)).suggestName("hwacha")
      }))
    }
  }
})

