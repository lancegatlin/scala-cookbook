package org.ldg.spark

import org.apache.spark.sql._
import org.ldg._
import org.ldg.jdbc.JdbcHelper

object SparkJdbcHelper {
  val JDBC_USER = "user"
  val JDBC_PASSWORD = "password"
}

trait SparkJdbcHelper extends JdbcHelper {
  import SparkJdbcHelper._
  
  lazy val jdbcProps = Map(
    JDBC_USER -> jdbcConfig.username,
    JDBC_PASSWORD -> jdbcConfig.password
  ).toProperties

  case class SparkJdbcCfgBuilder(
    tableName: String
  )(implicit
    sparkSession: SparkSession
  ) {
    def read() : DataFrame =
      sparkSession.read
        .jdbc(
          url = jdbcConfig.url,
          table = quote(tableName),
          properties = jdbcProps
        )

    def write(
      dataFrame: DataFrame,
      saveMode: SaveMode = SaveMode.ErrorIfExists
    ) : Unit =
      dataFrame.write
        .mode(saveMode)
        .jdbc(
          url = jdbcConfig.url,
          table = quote(tableName),
          connectionProperties = jdbcProps
        )

    def append(dataFrame: DataFrame) : Unit =
      write(dataFrame, SaveMode.Append)

    def overwrite(dataFrame: DataFrame) : Unit =
      write(dataFrame, SaveMode.Overwrite)
  }

  def withJdbcTable[A](
    tableName: String
  )(
    f: SparkJdbcCfgBuilder => A
  )(implicit
    sparkSession: SparkSession
  ) : A =
    f(SparkJdbcCfgBuilder(tableName))
}
