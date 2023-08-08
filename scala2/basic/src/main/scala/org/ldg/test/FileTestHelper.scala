package org.ldg.test

import java.nio.file.{Files, Path, Paths}

import com.typesafe.scalalogging.slf4j.LazyLogging

import scala.collection.JavaConverters._
import org.apache.commons.io.FileUtils
import org.ldg._
import org.scalatest.Matchers

import scala.io.Source
import scala.util.Random

object FileTestHelper extends LazyLogging {
  val tmpDirCache = new TSMap[String, Path]
  Runtime.getRuntime.addShutdownHook(new Thread {
    override def run(): Unit = {
      tmpDirCache.entrySet().asScala.foreach { entry =>
        val tmpDirPath = entry.getValue
        val tmpDir = tmpDirPath.toFile
        logger.info(s"Removing temp dir: $tmpDir")
        if(FileUtils.deleteQuietly(tmpDir) == false) {
          logger.warn(s"Failed to delete tmpdir=$tmpDirPath")
        }
      }
    }
  })
}

trait FileTestHelper
  extends Matchers
  with HasTestName
  with LazyLogging {
  import FileTestHelper._

  def mkTmpDir(name: String) : Path = {
    tmpDirCache.getOrCompute(name) { () =>
      val tmpdir = Files.createTempDirectory(s"$name-")
      logger.info(s"Created temp directory for test $name: $tmpdir")
      tmpdir
    }
  }

  def mkTmpDir(name: String, subDir: String) : Path = {
    val tmpDir = mkTmpDir(name)
    val subDirPath = Paths.get(s"$tmpDir/$subDir")
    subDirPath.toFile.mkdir()
    subDirPath
  }

  def getTestTmpDir() : Path = mkTmpDir(testName)
  def getTestTmpDir(subDir: String) : Path = mkTmpDir(testName, subDir)

  def withTmpDir[A](task: String)(f: Path => A) : A = {
    f(getTestTmpDir(s"$task-${Random.alphanumeric.take(6).mkString}"))
  }

  def ls(path: String) : List[java.io.File] = {
    {
      new java.io.File(path)
    }
      .listFiles
      .toList
  }

  def getResourceAsString(name: String) : Option[String] = {
    require(name.startsWith("/"), s"Resource name must start with slash(/): $name")
    Option(getClass.getResourceAsStream(name)).map { resIn =>
      Source.fromInputStream(resIn).mkString
    }
  }

  def getResourceAsOneLineString(name: String) : Option[String] =
    getResourceAsString(name).map(_.replaceAll("\\s+", " "))

  def requireResourceAsString(name: String) : String =
    getResourceAsString(name).getOrDie(s"Failed to find resource: $name")

  def requireResourceAsOneLineString(name: String) : String =
    getResourceAsOneLineString(name).getOrDie(s"Failed to find resource: $name")

  /**
    * Compare to strings line by line
    * @param lhs expect
    * @param rhs
    * @return
    */
  def compareLines(lhs: String, rhs: String) : Option[(Int,String,String)] = {
    val lhsLines = lhs.lines.iterator().asScala.toIndexedSeq
    val rhsLines = rhs.lines.iterator().asScala.toIndexedSeq
    lhsLines.iterator.zip(rhsLines.iterator).zipWithIndex.collectFirst {
      case ((lhsLine,rhsLine),idx) if lhsLine != rhsLine => (idx,lhsLine,rhsLine)
    }
    if(lhsLines.size < rhsLines.size) {
      val idx = lhsLines.size
      Some((idx,"<missing>",rhsLines(idx)))
    } else if(lhsLines.size > rhsLines.size) {
      val idx = rhsLines.size
      Some((idx,lhsLines(idx),"<missing>"))
    } else {
      None
    }
  }

  def ensureSameLines(expected: String, found: String) : Unit = {
    compareLines(expected, found).foreach { case (idx,expectedLine,foundLine) =>
      val prefix = s"[line ${idx+1}]"
      // note: this will always fail at this point
      (prefix,foundLine) shouldBe (prefix,expectedLine)
    }
  }

  def rmDir(path: String) : Unit = {
    val dir = new java.io.File(path)
    if(FileUtils.deleteQuietly(dir) == false) {
      logger.warn(s"Could not delete path=$path, will delete on exit")
      FileUtils.forceDeleteOnExit(dir)
    }
  }

  def copyResourceToFile(name: String, destPath: String) : String = {
    require(name.startsWith("/"), s"Resource name must start with slash(/): $name")
    val resIn = Option(getClass.getResourceAsStream(name)).getOrDie(s"Failed to find resource: $name")
    val fullDestPath = destPath + name
    FileUtils.copyInputStreamToFile(resIn, new java.io.File(fullDestPath))
    fullDestPath
  }
}
