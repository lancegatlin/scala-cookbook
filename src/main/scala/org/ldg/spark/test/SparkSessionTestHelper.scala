package org.ldg.spark.test

import com.typesafe.scalalogging.slf4j.LazyLogging
import org.apache.spark.sql.types.StructType
import org.apache.spark.sql.{DataFrame, SaveMode, SparkSession}
import org.ldg._
import org.ldg.test.FileTestHelper

import scala.collection.JavaConverters._
import scala.io.Source

object SparkSessionTestHelper extends LazyLogging {

  val sparkSessionCache = new TSMap[SparkTestConfig, SparkSession]

  case class SparkTestConfig(
    name: String,
    config: Seq[(String,String)]
  )

  Runtime.getRuntime.addShutdownHook(new Thread {
    override def run(): Unit = {
      sparkSessionCache.entrySet().asScala.foreach { entry =>
        val sparkTestCfg = entry.getKey
        val sparkSession = entry.getValue
        logger.info(s"Closing SparkSession for config ${sparkTestCfg.name}")
        sparkSession.close()
      }
    }
  })
}

trait SparkSessionTestHelper extends FileTestHelper with LazyLogging {
  import SparkSessionTestHelper._

  def sparkTestCfg: SparkTestConfig = SparkTestConfig("default", Seq.empty)

  /**
    * Construct a SparkSession, pass to provided function, then close the session
    * even if an exception is thrown
    *
    * @param f function to pass SparkSession to
    * @tparam A return type
    * @return value returned by function
    */
  def withSparkSession[A](f: SparkSession => A) : A = {
    val sparkSession =
      sparkSessionCache.getOrCompute(sparkTestCfg) { () =>
          val _sparkSession =
            SparkSession
              .builder()
              .master("local[*]")
              .appName("SparkTest")

          sparkTestCfg.config.foreach { case (k,v) =>
            _sparkSession.config(k,v)
          }
          logger.info(s"Creating SparkSession[${sparkTestCfg.name}]")
          _sparkSession.getOrCreate()
      }

    f(sparkSession)
  }

  /**
    * Write a DataFrame to a CSV String
    * @param df
    * @return output CSV String
    */
  def writeDfToCsvString(
    df: DataFrame,
    header: Boolean = false
  ) : String = {
    withTmpDir("write-df-to-csv") { tmpdir =>
      val outputDirPath = tmpdir.toString + "/output"
      df.coalesce(1).write
        .mode(SaveMode.Overwrite)
        .option("header", header.toString)
        .csv(outputDirPath)

//      logger.info(s"Wrote CSV ${df.count()} records to $outputDirPath")

      val outputFile =
        ls(outputDirPath)
          // note: should only be one
          .find(_.getName.endsWith(".csv"))
          .getOrDie(s"Did not find expected output CSV at $outputDirPath")

      Source.fromFile(outputFile).mkString
    }
  }

  def writeDfToCsvFile(df: DataFrame, path: String) : Unit = {
    val csvStr = writeDfToCsvString(df)
    new java.io.PrintWriter(path) { write(csvStr); close() }
  }

  /**
    * Convert a CSV string to DataFrame, transform and write back to CSV String
    * @param csv
    * @param f
    * @param sparkSession
    * @return output CSV String
    */
  def xfrmCsv(
    csv: String
  )(
    f: DataFrame => DataFrame
  )(implicit
    sparkSession: SparkSession
  ) : String = {
    import sparkSession.sqlContext.implicits._
    writeDfToCsvString {
      val df =
        sparkSession.read
          .option("header",true)
          .csv(csv.lines.iterator().asScala.toIndexedSeq.toDS())
      f(df)
    }
  }

  def csvToDF(
    csv: String,
    schema: StructType,
    header: Boolean = false
  )(implicit
    sparkSession: SparkSession
  ) : DataFrame = {
    import sparkSession.sqlContext.implicits._

    val csvLines = csv.lines.iterator().asScala.toList
    val csvDS =
      sparkSession.sparkContext
        .parallelize(csvLines)
        .toDS

    sparkSession.read.schema(schema).option("header",header.toString).csv(csvDS)
  }

  def requireCsvResourceAsDf(
    resourceName: String,
    schema: StructType
  )(implicit
    sparkSession: SparkSession
  ) : DataFrame = {
    val csv = requireResourceAsString(resourceName)
    csvToDF(csv, schema)
  }

  def compareDfToExpectedCsv(
    df: DataFrame,
    expectedCsvResourceName: String
  ) : Unit = {
    val csv = writeDfToCsvString(df)
    val expectedCsv = requireResourceAsString(expectedCsvResourceName)
    ensureSameLines(csv, expectedCsv)
  }
}
