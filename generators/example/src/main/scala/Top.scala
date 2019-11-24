package example

import chisel3._

import freechips.rocketchip.subsystem._
import freechips.rocketchip.system._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.util.DontTouch

import testchipip._

import utilities.{System, SystemModule}

import sifive.blocks.devices.gpio._

// ------------------------------------
// BOOM and/or Rocket Top Level Systems
// ------------------------------------

class Top(implicit p: Parameters) extends System
  with HasPeripheryGPIO // Optionally adds GPIO
  with CanHavePeripheryBlockDevice // Enables optionally adding the block device
  with CanHavePeripheryInitZero // Enables optionally adding the initzero example widget
  with CanHavePeripheryGCD // Enables optionally adding the GCD tilelink widget
  with CanHavePeripheryPWM // Enables optionally adding the PWM example port and widget
  with CanHavePeripherySerial // Enables optionally adding the TSI serial-adapter and port
{
  override lazy val module = new TopModule(this)
}

class TopModule[+L <: Top](l: L) extends SystemModule(l)
  with HasPeripheryGPIOModuleImp
  with CanHavePeripheryBlockDeviceModuleImp
  with CanHavePeripheryPWMModuleImp
  with CanHavePeripherySerialModuleImp
  with DontTouch


