package org.ldg.concurrent

import cats.effect.Async
import org.ldg.concurrent.ExecutionContextExt.OrgLdgConcurrentExecutionExt

import scala.concurrent.ExecutionContext

trait BaseFuturePlanSpec {
  implicit val ec: ExecutionContext = ExecutionContext.global.withUnhandledExceptionHandler { ex =>
    // scalastyle:off
    System.err.println( s"[ERROR]: unhandled exception: $ex" )
    // scalastyle:on
    throw ex
  }
  implicit val bec: BlockingExecutionContext = BlockingExecutionContext( ec )
  implicit val F: Async[FuturePlan] = new FuturePlanAsync()
}
