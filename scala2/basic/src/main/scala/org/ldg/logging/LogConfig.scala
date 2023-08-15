package org.ldg.logging

import com.typesafe.config.{Config, ConfigRenderOptions}
import com.typesafe.scalalogging.LazyLogging

import scala.jdk.CollectionConverters._

object ConfigUtils {
  /**
   * Log config as a long string of newline separated values to an arbitrary function ensuring keys with sensitive
   * values such as passwords or secrets are redacted in the output string
   *
   * @param cfg config to log
   * @param excludeKey function that returns TRUE if key should be excluded from output
   * @param redactKey function that returns TRUE if key's value should be redacted (i.e. replaced with ***)
   * @param configRenderOptions config printing options
   * @param log function to print config to (e.g. logger.info(_))
   */
  def logConfig(
      cfg: Config,
      excludeKey: String => Boolean = { _ => false },
      redactKey: String => Boolean = { key =>
        Seq( "passw", "secret" ).exists( key.contains )
      },
      configRenderOptions: ConfigRenderOptions = ConfigRenderOptions.concise().setFormatted( true )
  )(log: String => Unit): Unit =
    log {
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
