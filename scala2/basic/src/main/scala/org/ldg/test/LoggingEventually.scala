package org.ldg.test

import com.typesafe.scalalogging.LazyLogging
import org.scalatest.concurrent.Eventually
import org.scalatest.enablers.Retrying

trait LoggingEventually extends Eventually with LazyLogging {

  def eventually[A](
    msg: String,
    info: String => Unit = logger.info(_),
    debug: String => Unit = logger.debug(_)
  )(
    f: => A
  )( implicit
     config: PatienceConfig,
     retrying: Retrying[A],
     pos: org.scalactic.source.Position
  ): A = {
    info( s"eventually $msg" )
    val result = super.eventually {
      try f catch { case ex:Throwable =>
        debug( s"eventually $msg" )
        throw ex
      }
    }( config, retrying, pos )
    info( msg )
    result
  }

}
