package org.ldg.spark.test

import java.sql.Timestamp
import java.time.Instant

import com.holdenkarau.spark.testing.{DataframeGenerator, Column => SingleColumnGen}
import com.typesafe.scalalogging.slf4j.LazyLogging
import org.apache.spark.sql._
import org.apache.spark.sql.types.StructType
import org.ldg._
import org.scalacheck.rng.Seed
import org.scalacheck.{Arbitrary, Gen}

object SparkGenDfTestHelper {

  object Generators {
    // note: these need to be outside trait since Gen is lazy and closes
    // over functions below (which could cause test object to be serialized

    def genStrN(n: Int) : Gen[String] =
      Gen.listOfN(n, Gen.alphaChar).map(_.mkString)

    def genStrRngN(min:Int, max: Int) : Gen[String] =
      for {
        n <- Gen.choose(min,max)
        str <- genStrN(n)
      } yield str

    def genTimestamp(when: Instant) : Gen[Timestamp] =
      Gen.const(Timestamp.from(when))

    def genCurrency() : Gen[BigDecimal] =
      Gen.choose(1, 1000000).map(c => BigDecimal(c,2))
  }
}

trait SparkGenDfTestHelper extends LazyLogging {

  def mkGenDataFrame(
    schema: StructType,
    numOfPartitions: Int = 1,
    customColumnGenerators: Seq[(String,Gen[Any])] = Seq.empty
  )(implicit
    sparkSession: SparkSession
  ) : Gen[DataFrame] = {
    val _customColumnGenerators =
      customColumnGenerators.map { case (colName, colGen) =>
        new SingleColumnGen(colName,colGen)
      }

    val arbitraryDataFrame : Arbitrary[DataFrame] =
      DataframeGenerator.arbitraryDataFrameWithCustomFields(
        sqlContext = sparkSession.sqlContext,
        schema = schema,
        minPartitions = numOfPartitions
      )(_customColumnGenerators:_*)

    arbitraryDataFrame.arbitrary
  }

  def runGenDf(
    gen: Gen[DataFrame],
    n: Int,
    seed : Seed = Seed.random()
  ) : DataFrame = {
    gen.apply(Gen.Parameters.default.withSize(n), seed)
      .getOrDie("Expected Gen[DataFrame] to generate a value")
  }
}
