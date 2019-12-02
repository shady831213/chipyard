package example

import chisel3._

import freechips.rocketchip.config.{Config}

// ---------------------
// BOOM Configs
// ---------------------
//fixme:Boom not support chisel3.2.0

//class SmallBoomConfig extends Config(
//  new WithTop ++                                            // use normal top
//  new WithBootROM ++                                        // use testchipip bootrom
//  new freechips.rocketchip.subsystem.WithInclusiveCache ++  // use SiFive L2 cache
//  new boom.common.WithSmallBooms ++                         // 1-wide BOOM
//  new boom.common.WithNBoomCores(1) ++                      // single-core
//  new freechips.rocketchip.system.BaseConfig)               // "base" rocketchip system
//
//class MediumBoomConfig extends Config(
//  new WithTop ++
//  new WithBootROM ++
//  new freechips.rocketchip.subsystem.WithInclusiveCache ++
//  new boom.common.WithMediumBooms ++                        // 2-wide BOOM
//  new boom.common.WithNBoomCores(1) ++
//  new freechips.rocketchip.system.BaseConfig)
//
//class LargeBoomConfig extends Config(
//  new WithTop ++
//  new WithBootROM ++
//  new freechips.rocketchip.subsystem.WithInclusiveCache ++
//  new boom.common.WithLargeBooms ++                         // 3-wide BOOM
//  new boom.common.WithNBoomCores(1) ++
//  new freechips.rocketchip.system.BaseConfig)
//
//class MegaBoomConfig extends Config(
//  new WithTop ++
//  new WithBootROM ++
//  new freechips.rocketchip.subsystem.WithInclusiveCache ++
//  new boom.common.WithMegaBooms ++                          // 4-wide BOOM
//  new boom.common.WithNBoomCores(1) ++
//  new freechips.rocketchip.system.BaseConfig)
//
//class DualSmallBoomConfig extends Config(
//  new WithTop ++
//  new WithBootROM ++
//  new freechips.rocketchip.subsystem.WithInclusiveCache ++
//  new boom.common.WithSmallBooms ++
//  new boom.common.WithNBoomCores(2) ++                      // dual-core
//  new freechips.rocketchip.system.BaseConfig)
//
//class SmallRV32BoomConfig extends Config(
//  new WithTop ++
//  new WithBootROM ++
//  new freechips.rocketchip.subsystem.WithInclusiveCache ++
//  new boom.common.WithoutBoomFPU ++                        // no fp
//  new boom.common.WithBoomRV32 ++                          // rv32 (32bit)
//  new boom.common.WithSmallBooms ++
//  new boom.common.WithNBoomCores(1) ++
//  new freechips.rocketchip.system.BaseConfig)
