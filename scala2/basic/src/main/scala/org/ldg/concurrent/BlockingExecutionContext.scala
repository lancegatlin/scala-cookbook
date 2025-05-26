package org.ldg.concurrent

import scala.concurrent.ExecutionContext

/**
  * An ExecutionContext type for implicitly requiring an ExecutionContext that is suitable for blocking operations.
  */
case class BlockingExecutionContext( executionContext: ExecutionContext )
