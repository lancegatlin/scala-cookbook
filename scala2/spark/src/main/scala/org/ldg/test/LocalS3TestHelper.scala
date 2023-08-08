package org.ldg.test

import org.apache.commons.io.FilenameUtils
import org.ldg.spark.SparkS3Helper

trait LocalS3TestHelper
  extends HasTestName
  with FileTestHelper
  with SparkS3Helper
{
  lazy val localS3Dir = FilenameUtils.separatorsToUnix(getTestTmpDir("s3").toString)

  override def mkS3Path(bucket: String, path: String): String = {
    s"file:///$localS3Dir/$bucket/$path"
  }
}
