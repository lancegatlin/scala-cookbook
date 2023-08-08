package org.ldg.logging

import com.typesafe.config.{Config, ConfigRenderOptions}
import com.typesafe.scalalogging.LazyLogging

import scala.jdk.CollectionConverters._
trait LogConfig extends LazyLogging {
    def logConfig(
      cfg: Config,
      excludeKey: String => Boolean = { key => false },
      redactKey: String => Boolean = { key => Seq( "passw", "secret" ).exists( key.contains ) },
      configRenderOptions: ConfigRenderOptions = ConfigRenderOptions.concise().setFormatted( true )
  ): Unit =
    logger.info {
      "Config:\n" +
        cfg
          .entrySet()
          .asScala
          .filterNot( entry => excludeKey( entry.getKey ) )
          .toArray
          .sortBy( _.getKey )
          .iterator
          .map { entry =>
            import entry._
            if (redactKey( getKey ) == false) {
              s"$getKey -> ${getValue.render( configRenderOptions )}"
            } else {
              if (getValue.render( configRenderOptions ) != "\"\"") {
                s"$getKey -> ***"
              } else {
                s"$getKey -> ***empty***"
              }
            }
          }
          .mkString( "\n" )
    }
}
