package org.ldg.test

trait MiscTestHelper {
  def maskPassword(password: String) : String =
    if(password.nonEmpty) {
      "***"
    } else {
      "<not set>"
    }
}
