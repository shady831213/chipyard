package example

import chisel3._

import freechips.rocketchip.config.{Parameters}
import freechips.rocketchip.util.{GeneratorApp}
import utilities.TestSuiteHelper

object Generator extends GeneratorApp {
  // add unique test suites
  override def addTestSuites {
    implicit val p: Parameters = params
    TestSuiteHelper.addRocketTestSuites
//    TestSuiteHelper.addBoomTestSuites
  }

  // specify the name that the generator outputs files as
  override lazy val longName = names.topModuleProject + "." + names.topModuleClass + "." + names.configs

  // generate files
  generateFirrtl
  generateAnno
  generateTestSuiteMakefrags
  generateArtefacts
}
