package org.ldg.test

import java.sql.Connection

import com.typesafe.scalalogging.slf4j.LazyLogging

import scala.collection.JavaConverters._
import com.zaxxer.hikari.{HikariConfig, HikariDataSource}
import org.ldg._
import org.ldg.jdbc.{JdbcConfig, JdbcHelper}

object JdbcTestHelper extends LazyLogging {

  def runSql(name: String, sql: String)(implicit connection: Connection): Unit = {
    logger.info(s"Running sql: $name")
    connection.prepareStatement(sql).executeUpdate()
  }

  def maybeRunSql(name: String, optSql: Option[String])(implicit connection: Connection): Unit = {
    optSql match {
      case Some(sql) => runSql(name, sql)
      case None =>
        logger.info(s"Skipping empty sql: $name")
    }
  }

  case class RunScriptSql(
    asyncClean: () => Unit,
    asyncUnInit: () => Unit
  )

  // note: global to track different data sources & initSql across multiple tests
  private val jdbcCfgToDataSource = new TSMap[JdbcConfig, HikariDataSource]()
  private val scriptIsInit = new TSMap[String, RunScriptSql]()

  Runtime.getRuntime.addShutdownHook(new Thread {
    override def run(): Unit = {
      scriptIsInit.entrySet().asScala.foreach { entry =>
        entry.getValue.asyncUnInit()
      }
      jdbcCfgToDataSource.entrySet().asScala.foreach { entry =>
        entry.getValue.close()
      }
    }
  })
}


trait JdbcTestHelper
  extends JdbcHelper
  with FileTestHelper
  with MiscTestHelper
  with LazyLogging
{
  import JdbcTestHelper._

  def mkConnection() : Connection = {
    val dataSource = jdbcCfgToDataSource.getOrCompute(jdbcConfig) { () =>
      logger.info(s"Creating HikariDataSource for test[$testName]: url=${jdbcConfig.url} username=${jdbcConfig.username} password=${maskPassword(jdbcConfig.password)}")
      val config = new HikariConfig
      config.setJdbcUrl(jdbcConfig.url)
      config.setUsername(jdbcConfig.username)
      config.setPassword(jdbcConfig.password)

      new HikariDataSource(config)
    }

    dataSource.getConnection()
  }

  def withJdbc[A](f: Connection => A) : A = {
    val connection = mkConnection()
    try {
      f(connection)
    } finally {
      connection.close()
    }
  }

  def ensureInitDb(scripts: Seq[String])(implicit connection: Connection) : Unit = {
    scripts.foreach { script =>
      scriptIsInit.getOrCompute(script) { () =>
        val initSqlName = s"$script.init.sql"
        val cleanSqlName = s"$script.clean.sql"
        val unInitSqlName = s"$script.uninit.sql"
        val optInitSql = getResourceAsString(s"/$initSqlName")
        val optCleanSql = getResourceAsString(s"/$cleanSqlName")
        val optUnInitSql = getResourceAsString(s"/$unInitSqlName")

        maybeRunSql(initSqlName, optInitSql)

        RunScriptSql(
          asyncClean = {() =>
            withJdbc { implicit connection =>
              maybeRunSql(cleanSqlName, optCleanSql)
            }
          },
          asyncUnInit = { () =>
            withJdbc { implicit connection =>
              maybeRunSql(unInitSqlName, optUnInitSql)
            }
          }
        )
      }
    }
  }

  def cleanDb(scripts: Seq[String])(implicit connection: Connection) : Unit = {
    scripts.foreach { script =>
      scriptIsInit.get(script).asyncClean()
    }
  }

  def withInitDb[A](scripts: String*)(f: Connection => A) : A = {
    withJdbc { implicit connection =>
      try {
        ensureInitDb(scripts)
        cleanDb(scripts)
        f(connection)
      } finally {
      }
    }
  }

  def countTable(table: String)(implicit connection: Connection) : Long = {
    // note: need quotes for table names that include underscore for postgres
    val sql = s"SELECT COUNT(*) FROM ${quote(table)}"
    val result =
      connection
        .prepareStatement(sql)
        .executeQuery()
    if(result.next()) {
      result.getLong(1)
    } else {
      die(s"Expected count from SQL: $sql")
    }
  }
  // todo: table to CSV?
}
