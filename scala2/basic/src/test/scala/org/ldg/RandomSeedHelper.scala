package org.ldg

import com.typesafe.scalalogging.StrictLogging
import org.scalatest._
import org.scalatest.events.{Event, ExceptionalEvent}

/**
  * A trait for repeating tests with a new random seed for each test run
  *  - repeats all tests a configured number of times with a new seed for each run
  *  - fails on first failure
  *  - ensures failing random seed is reported for reproducing
  * note: could be replaced with ScalaCheck
  */
trait RandomSeedHelper extends TestSuiteMixin { self: TestSuite with StrictLogging =>
  def repeatedTestCount: Int = 100

  /**
    * Override with fixed value for troubleshooting intermittent failure
    * @return a pseudo-random seed
    */
  def calcRandomSeed: Long = scala.util.Random.nextLong()

  def setRandomSeed( seed: Long ): Unit

  private def repeatTests( testName: String, args: Args ): ( Status, Long ) =
    (0 until repeatedTestCount).foldLeft[( Status, Long )]( ( SucceededStatus, 0 ) ) {
      case ( tuple @ ( FailedStatus, _ ), _ ) => tuple
      case ( _, times ) =>
        val reporter = new Reporter {
          override def apply( event: Event ): Unit = event match {
            // always report failure
            case _: ExceptionalEvent => args.reporter( event )
            // report last success
            case _ if times == repeatedTestCount - 1 => args.reporter( event )
            case _                                   => // otherwise do nothing
          }
        }
        // each test run gets a new seed
        val randomSeed = calcRandomSeed
        setRandomSeed( randomSeed )
        val status = super.runTest(
          testName,
          args.copy(
            reporter = reporter
          ) )
        ( status, randomSeed )
    }
  abstract override protected def runTest( testName: String, args: Args ): Status = {
    val ( finalStatus, finalRandomSeed ) = repeatTests( testName, args )
    finalStatus match {
      case failedStatus @ FailedStatus =>
        val msg = s"Test failures with randomSeed=$finalRandomSeed"
        logger.error( msg )
        //scalastyle:off
        Console.err.println( msg ) // sometimes logging turned off by default
        //scalastyle:on
        failedStatus
      case status => status
    }
  }
}
