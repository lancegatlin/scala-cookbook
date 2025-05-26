package org.ldg.concurrent

import scala.concurrent.ExecutionContext

object ExecutionContextExt {
  implicit class OrgLdgConcurrentExecutionExt(val self: ExecutionContext ) extends AnyVal {
    def withUnhandledExceptionHandler(
        handler: Throwable => Unit
    ): ExecutionContext =
      new ExecutionContext {
        override def execute( runnable: Runnable ): Unit = self.execute( runnable )
        override def reportFailure( cause: Throwable ): Unit = handler( cause )
      }
  }
}
