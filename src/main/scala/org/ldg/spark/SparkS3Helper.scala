package org.ldg.spark

import org.apache.hadoop.fs.{FileStatus, FileSystem, Path}
import org.apache.spark.sql._
import org.apache.spark.sql.types.StructType
import org.ldg._

trait SparkS3Helper {

  // Hook point for tests to inject local path
  def mkS3Path(bucket: String, path: String) : String =
    s"s3://$bucket/$path"

  case class SparkReadS3CfgBuilder(
    s3Bucket: String,
    optHeader: Option[Boolean] = None,
    optSchema: Option[StructType] = None
  )(implicit
    sparkSession: SparkSession
  ) {
    def schema(schema: StructType) = copy(optSchema = Some(schema))
    def header(header: Boolean) = copy(optHeader = Some(header))
    def configure(dfr: DataFrameReader) : DataFrameReader = {
      dfr
        .transform { _dfr =>
          optHeader match {
            case Some(header) => _dfr.option("header", header)
            case None => _dfr
          }
        }
        .transform { _dfr =>
          optSchema match {
            case Some(schema) => _dfr.schema(schema)
            case None => _dfr
          }
        }
    }

    def resolvePaths(paths: Seq[String]) : Seq[String] =
      paths.map(mkS3Path(s3Bucket, _))

    def csv(paths: String*) : DataFrame  =
      configure(sparkSession.read)
        .csv(resolvePaths(paths):_*)

    def parquet(paths: String*) : DataFrame  =
      configure(sparkSession.read)
        .parquet(resolvePaths(paths):_*)
  }

  case class SparkWriteS3CfgBuilder(
    s3Bucket: String,
    optSaveMode: Option[SaveMode] = None
  )(implicit
    sparkSession: SparkSession
  ) {
    def mode(saveMode: SaveMode) = copy(optSaveMode = Some(saveMode))

    def configure[A](dfw: DataFrameWriter[A]) : DataFrameWriter[A] = {
      dfw
        .transform { _dfw =>
          optSaveMode match {
            case Some(saveMode) => _dfw.mode(saveMode)
            case None => _dfw
          }
        }
    }

    def csv(dataFrame: DataFrame, path: String) : Unit  =
      configure(dataFrame.write)
        .csv(mkS3Path(s3Bucket, path))

    def parquet(dataFrame: DataFrame, path: String) : Unit =
      configure(dataFrame.write)
        .parquet(mkS3Path(s3Bucket, path))
  }

  case class SparkS3CfgBuilder(
    s3Bucket: String
  )(implicit
    sparkSession: SparkSession
  ) {
    lazy val hadoopFs = getHadoopFs()

    def glob(path: String) : Array[FileStatus] =
      s3Glob(s3Bucket, path)(hadoopFs)

    def globExists(path: String) : Boolean =
      s3GlobExists(s3Bucket, path)(hadoopFs)

    def read = SparkReadS3CfgBuilder(s3Bucket)
    def write = SparkWriteS3CfgBuilder(s3Bucket)
  }

  def withS3[A](s3Bucket: String)(f: SparkS3CfgBuilder => A)(implicit sparkSession: SparkSession) : A =
    f(SparkS3CfgBuilder(s3Bucket))

  def getHadoopFs()(implicit sparkSession: SparkSession) : FileSystem =
    FileSystem.get(sparkSession.sparkContext.hadoopConfiguration)

  def s3GlobExists(s3Bucket: String, path: String)(implicit hadoopFs: FileSystem) : Boolean = {
    s3Glob(s3Bucket, path).nonEmpty
  }

  def s3Glob(s3Bucket: String, path: String)(implicit hadoopFs: FileSystem) : Array[FileStatus] = {
    val fullPath = mkS3Path(s3Bucket, path)
    hadoopFs.globStatus(new Path(fullPath))
  }
}
