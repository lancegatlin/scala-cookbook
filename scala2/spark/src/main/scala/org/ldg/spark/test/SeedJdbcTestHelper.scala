package org.ldg.spark.test

import org.apache.spark.sql.{DataFrame, SparkSession}
import org.ldg.spark.SparkJdbcHelper
import org.ldg.test.JdbcTestHelper
import org.scalacheck.Gen

trait SeedJdbcTestHelper
  extends SparkJdbcHelper
  with JdbcTestHelper
  with SparkGenDfTestHelper
{

  def seedJdbcTable(
    gen: Gen[DataFrame],
    n: Int,
    outputTableName: String
  )(implicit
    sparkSession: SparkSession
  ) : Unit = {
    val df = runGenDf(gen, n)
    withJdbcTable(outputTableName)(_.overwrite(df))
  }
}
