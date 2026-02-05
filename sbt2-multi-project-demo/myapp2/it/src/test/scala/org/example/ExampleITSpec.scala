package org.example

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class MyApp2ITSpec extends AnyFlatSpec with Matchers {
  val notUsed = new MyApp2ExampleClass()
  println(notUsed.getClass.getSimpleName)
  println("MyApp2ITSpec")
}
