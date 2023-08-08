package org.ldg.test

trait HasTestName {
  def testName: String = this.getClass.getSimpleName
}
