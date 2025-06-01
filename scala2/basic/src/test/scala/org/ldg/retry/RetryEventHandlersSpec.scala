package org.ldg.retry

import cats.effect.IO
import cats.effect.std.Console
import cats.effect.unsafe.implicits.global
import cats.{Id, Show}
import org.ldg.retry.RetryEventHandlers.{catsConsoleLogging, catsLogging, retryEventToLogMessage, slf4jLogEvent, stderrLogging}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.slf4j.event.{SubstituteLoggingEvent, Level => Slf4jLevel}
import org.slf4j.helpers.SubstituteLogger
import org.typelevel.log4cats.{Logger => CatsLogger}

import java.nio.charset.Charset
import java.util.concurrent.ConcurrentLinkedQueue
import scala.concurrent.duration.DurationInt
import scala.jdk.CollectionConverters.IteratorHasAsScala

class RetryEventHandlersSpec extends AnyWordSpec with Matchers {

  "retryEventToLogMessage" should {
    "format retry success message" in {
      val retryState = RetryState.initial( 3, None, "test-correlation-id" )( java.time.Instant.now )
      val event = RetryEvent.OnRetrySuccess( retryState )
      val ( level, message ) = retryEventToLogMessage( RetryState.fmtRetryState )( event )

      level shouldBe Slf4jLevel.INFO
      message shouldBe "Retry success (attempts=0/3 elapsed=0 milliseconds/inf correlationId=test-correlation-id)"
    }

    "format retry failure message with stop reason" in {
      val retryState = RetryState.initial( 3, None, "test-correlation-id" )( java.time.Instant.now )
      val event = RetryEvent.OnAfterFailure( retryState, new RuntimeException( "failure" ), RetryAction.StopRetry( FailedRetryReason.ExhaustedMaxAttempts ) )
      val ( level, message ) = retryEventToLogMessage( RetryState.fmtRetryState )( event )

      level shouldBe Slf4jLevel.WARN
      //scalastyle:off
      message shouldBe "Retry ExhaustedMaxAttempts (attempts=0/3 elapsed=0 milliseconds/inf correlationId=test-correlation-id) rethrowing: java.lang.RuntimeException: failure"
      //scalastyle:on
    }

    "format retry failure message with attempt again reason" in {
      val retryState = RetryState.initial( 3, None, "test-correlation-id" )( java.time.Instant.now )
      val event = RetryEvent.OnAfterFailure( retryState, new RuntimeException( "failure" ), RetryAction.AttemptAgain( 1.second ) )
      val ( level, message ) = retryEventToLogMessage( RetryState.fmtRetryState )( event )

      level shouldBe Slf4jLevel.INFO
      //scalastyle:off
      message shouldBe "Retrying failure (attempts=0/3 elapsed=0 milliseconds/inf correlationId=test-correlation-id) after 1 second delay: java.lang.RuntimeException: failure"
      //scalastyle:on
    }
  }

  "slf4jLogEvent" should {
    class Fixture {
      val eventQueue: java.util.concurrent.ConcurrentLinkedQueue[SubstituteLoggingEvent] =
        new java.util.concurrent.ConcurrentLinkedQueue[SubstituteLoggingEvent]()
      val fakeLogger = new SubstituteLogger( "fakeLogger", eventQueue, false )
      val fixture = slf4jLogEvent[IO]( fakeLogger )
    }

    "log retry success with INFO level" in new Fixture {
      val retryState = RetryState.initial( 3, None, "test-correlation-id" )( java.time.Instant.now )
      val event = RetryEvent.OnRetrySuccess( retryState )

      fixture( event ).unsafeRunSync()
      val events = eventQueue.iterator().asScala.toList
      events.size shouldBe 1
      events.head.getLevel shouldBe Slf4jLevel.INFO
      events.head.getMessage should include( "Retry success" )
      Option( events.head.getThrowable ) shouldBe None
    }

    "log retry failure with WARN level when retry stops" in new Fixture {
      val retryState = RetryState.initial( 3, None, "test-correlation-id" )( java.time.Instant.now )
      val ex = new RuntimeException( "failure" )
      val event = RetryEvent.OnAfterFailure( retryState, ex, RetryAction.StopRetry( FailedRetryReason.ExhaustedMaxAttempts ) )

      fixture( event ).unsafeRunSync()
      val events = eventQueue.iterator().asScala.toList
      events.size shouldBe 1
      events.head.getLevel shouldBe Slf4jLevel.WARN
      events.head.getMessage should include( "Retry ExhaustedMaxAttempts" )
      Option( events.head.getThrowable ) shouldBe Some( ex )
    }

    "log retry failure with INFO level when retry continues" in new Fixture {
      val retryState = RetryState.initial( 3, None, "test-correlation-id" )( java.time.Instant.now )
      val ex = new RuntimeException( "failure" )
      val event = RetryEvent.OnAfterFailure( retryState, ex, RetryAction.AttemptAgain( 1.second ) )

      fixture( event ).unsafeRunSync()
      val events = eventQueue.iterator().asScala.toList
      events.size shouldBe 1
      events.head.getLevel shouldBe Slf4jLevel.INFO
      events.head.getMessage should include( "Retrying failure" )
      Option( events.head.getThrowable ) shouldBe Some( ex )
    }
  }

  "stderrLogging" should {
    class Fixture {
      val capturedStderr = scala.collection.mutable.ArrayBuffer[String]()
      val capturingStderr = new java.io.PrintStream( new java.io.ByteArrayOutputStream() {
        override def write( b: Array[Byte], off: Int, len: Int ): Unit =
          capturedStderr += new String( b, off, len, Charset.defaultCharset() )
      } )

      val fixture = stderrLogging[Id]( capturingStderr )
    }

    "log INFO retry success to stderr" in new Fixture {
      val retryState = RetryState.initial( 3, None, "test-correlation-id" )( java.time.Instant.now )
      val event = RetryEvent.OnRetrySuccess( retryState )

      fixture( event )
      capturedStderr.size shouldBe 1
      capturedStderr.exists( _.contains( "[INFO]: Retry success" ) ) shouldBe true
    }

    "log WARN retry failure to stderr when retry stops" in new Fixture {
      val retryState = RetryState.initial( 3, None, "test-correlation-id" )( java.time.Instant.now )
      val event = RetryEvent.OnAfterFailure( retryState, new RuntimeException( "failure" ), RetryAction.StopRetry( FailedRetryReason.ExhaustedMaxAttempts ) )

      fixture( event )
      capturedStderr.size shouldBe 1
      capturedStderr.exists( _.contains( "[WARN]: Retry ExhaustedMaxAttempts" ) ) shouldBe true
    }

    "log INFO retry failure to stderr when retry continues" in new Fixture {
      val retryState = RetryState.initial( 3, None, "test-correlation-id" )( java.time.Instant.now )
      val event = RetryEvent.OnAfterFailure( retryState, new RuntimeException( "failure" ), RetryAction.AttemptAgain( 1.second ) )

      fixture( event )
      capturedStderr.size shouldBe 1
      capturedStderr.exists( _.contains( "[INFO]: Retrying failure" ) ) shouldBe true
    }
  }

  "catsConsoleLogging" should {
    class Fixture {
      val capturedStdout = scala.collection.mutable.ArrayBuffer[String]()
      val capturedStderr = scala.collection.mutable.ArrayBuffer[String]()
      implicit val fakeConsole: Console[IO] = new Console[IO] {
        override def readLineWithCharset( charset: Charset ): IO[String] = ???
        override def print[A]( a: A )( implicit S: Show[A] ): IO[Unit] =
          IO( capturedStdout += S.show( a ) )
        // scalastyle:off
        override def println[A]( a: A )( implicit S: Show[A] ): IO[Unit] =
          IO( capturedStdout += S.show( a ) + "\n" )
        // scalastyle:on
        override def error[A]( a: A )( implicit S: Show[A] ): IO[Unit] =
          IO( capturedStderr += S.show( a ) )
        override def errorln[A]( a: A )( implicit S: Show[A] ): IO[Unit] =
          IO( capturedStderr += S.show( a ) + "\n" )
      }

      val fixture = catsConsoleLogging[IO]()
    }

    "log INFO retry success to stderr" in new Fixture {
      val retryState = RetryState.initial( 3, None, "test-correlation-id" )( java.time.Instant.now )
      val event = RetryEvent.OnRetrySuccess( retryState )

      fixture( event ).unsafeRunSync()
      capturedStdout shouldBe empty
      capturedStderr.size shouldBe 1
      capturedStderr.exists( _.contains( "[INFO]: Retry success" ) ) shouldBe true
    }

    "log WARN retry failure to stderr when retry stops" in new Fixture {
      val retryState = RetryState.initial( 3, None, "test-correlation-id" )( java.time.Instant.now )
      val event = RetryEvent.OnAfterFailure( retryState, new RuntimeException( "failure" ), RetryAction.StopRetry( FailedRetryReason.ExhaustedMaxAttempts ) )

      fixture( event ).unsafeRunSync()
      capturedStdout shouldBe empty
      capturedStderr.size shouldBe 1
      capturedStderr.exists( _.contains( "[WARN]: Retry ExhaustedMaxAttempts" ) ) shouldBe true
    }

    "log INFO retry failure to stderr when retry continues" in new Fixture {
      val retryState = RetryState.initial( 3, None, "test-correlation-id" )( java.time.Instant.now )
      val event = RetryEvent.OnAfterFailure( retryState, new RuntimeException( "failure" ), RetryAction.AttemptAgain( 1.second ) )

      fixture( event ).unsafeRunSync()
      capturedStdout shouldBe empty
      capturedStderr.size shouldBe 1
      capturedStderr.exists( _.contains( "[INFO]: Retrying failure" ) ) shouldBe true
    }
  }

  "catsLogging" should {
    class Fixture {
      val _capturedLogs = new ConcurrentLinkedQueue[( Slf4jLevel, String, Option[Throwable] )]()
      def capturedLogs: List[( Slf4jLevel, String, Option[Throwable] )] =
        _capturedLogs.iterator().asScala.toList
      implicit val fakeLogger: CatsLogger[IO] = new CatsLogger[IO] {
        private def captureLog( level: Slf4jLevel, message: String, t: Throwable ): IO[Unit] =
          IO( _capturedLogs.add( ( level, message, Some( t ) ) ) )
        private def log( level: Slf4jLevel, message: String ): IO[Unit] =
          IO( _capturedLogs.add( ( level, message, None ) ) )
        override def error( t: Throwable )( message: => String ): IO[Unit] =
          captureLog( Slf4jLevel.ERROR, message, t )
        override def warn( t: Throwable )( message: => String ): IO[Unit] =
          captureLog( Slf4jLevel.WARN, message, t )
        override def info( t: Throwable )( message: => String ): IO[Unit] =
          captureLog( Slf4jLevel.INFO, message, t )
        override def debug( t: Throwable )( message: => String ): IO[Unit] =
          captureLog( Slf4jLevel.DEBUG, message, t )
        override def trace( t: Throwable )( message: => String ): IO[Unit] =
          captureLog( Slf4jLevel.TRACE, message, t )
        override def error( message: => String ): IO[Unit] =
          log( Slf4jLevel.ERROR, message )
        override def warn( message: => String ): IO[Unit] =
          log( Slf4jLevel.WARN, message )
        override def info( message: => String ): IO[Unit] =
          log( Slf4jLevel.INFO, message )
        override def debug( message: => String ): IO[Unit] =
          log( Slf4jLevel.DEBUG, message )
        override def trace( message: => String ): IO[Unit] =
          log( Slf4jLevel.TRACE, message )
      }

      val fixture = catsLogging[IO]()
    }

    "log INFO retry success to stderr" in new Fixture {
      val retryState = RetryState.initial( 3, None, "test-correlation-id" )( java.time.Instant.now )
      val event = RetryEvent.OnRetrySuccess( retryState )

      fixture( event ).unsafeRunSync()
      capturedLogs.size shouldBe 1
      capturedLogs.head._1 shouldBe Slf4jLevel.INFO
      capturedLogs.head._2 should include( "Retry success" )
      capturedLogs.head._3 shouldBe None
    }

    "log WARN retry failure to stderr when retry stops" in new Fixture {
      val retryState = RetryState.initial( 3, None, "test-correlation-id" )( java.time.Instant.now )
      val ex = new RuntimeException( "failure" )
      val event = RetryEvent.OnAfterFailure( retryState, ex, RetryAction.StopRetry( FailedRetryReason.ExhaustedMaxAttempts ) )

      fixture( event ).unsafeRunSync()
      capturedLogs.size shouldBe 1
      capturedLogs.head._1 shouldBe Slf4jLevel.WARN
      capturedLogs.head._2 should include( "Retry ExhaustedMaxAttempts" )
      capturedLogs.head._3 shouldBe Some( ex )
    }

    "log INFO retry failure to stderr when retry continues" in new Fixture {
      val retryState = RetryState.initial( 3, None, "test-correlation-id" )( java.time.Instant.now )
      val ex = new RuntimeException( "failure" )
      val event = RetryEvent.OnAfterFailure( retryState, ex, RetryAction.AttemptAgain( 1.second ) )

      fixture( event ).unsafeRunSync()
      capturedLogs.size shouldBe 1
      capturedLogs.head._1 shouldBe Slf4jLevel.INFO
      capturedLogs.head._2 should include( "Retrying failure" )
      capturedLogs.head._3 shouldBe Some( ex )
    }
  }
}
