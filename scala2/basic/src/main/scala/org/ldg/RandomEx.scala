package org.ldg

/**
  * A class that adds methods to scala.util.Random
  * note: specifically not using value-class to ensure all methods including extension methods can be imported, i.e.
  *   import RandomEx._
  *  or inherited/overridden:
  *    class MyRandom extends RandomEx { ... }
  */
class RandomEx extends scala.util.Random {
  def stringMaxLength: Int = 10
  def seqMaxSize: Int = 3

  def nextPrintableString( length: Int = stringMaxLength ) =
    Iterator.fill( length )( nextPrintableChar() ).mkString

  def nextBigDecimal(): BigDecimal = BigDecimal( nextDouble() )

  def nextShort(): Short =
    (nextInt( Short.MaxValue.toInt - Short.MinValue.toInt + 1 ) + Short.MinValue.toInt).toShort

  def nextOption[A]( a: => A ): Option[A] = Option.when( nextBoolean() )( a )

  def nextSeq[A]( size: Int = nextInt( seqMaxSize ) )(a: => A ): Seq[A] = Seq.fill( size )( a )
}

object RandomEx extends RandomEx

