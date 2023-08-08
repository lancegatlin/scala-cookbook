package org.ldg.test

import java.util.concurrent.TimeUnit
import java.util.function.{Function => JavaFunction}

import com.couchbase.client.java.{Bucket, Cluster, CouchbaseCluster}
import com.couchbase.client.java.bucket.BucketType
import com.couchbase.client.java.cluster._
import com.couchbase.client.java.env.{CouchbaseEnvironment, DefaultCouchbaseEnvironment}
import com.couchbase.client.java.query.N1qlQuery
import com.typesafe.scalalogging.slf4j.LazyLogging

import scala.collection.JavaConverters._
import scala.concurrent.duration.Duration
import scala.concurrent.duration._

object CouchbaseTestHelper {
  case class CouchbaseCfg(
    nodes: List[String],
    username: String,
    password: String
  )

  case class CouchbaseConnection(
    env: CouchbaseEnvironment,
    cluster: Cluster,
    clusterManager: ClusterManager,
    tempBuckets: List[String]
  )

  case class ConnectCfg(
    firstNode: String,
    username: String
  )
  object ConnectCfg {
    def apply(couchbaseCfg: CouchbaseCfg) : ConnectCfg =
      ConnectCfg(
        firstNode = couchbaseCfg.nodes.head,
        username = couchbaseCfg.username
      )
  }
  // Note: global connection to share across tests since couchbase
  // client is slow to init
  val connections = new java.util.concurrent.ConcurrentHashMap[ConnectCfg,CouchbaseConnection]()

  // Only close connections on JVM shutdown
  Runtime.getRuntime.addShutdownHook(new Thread {
    override def run(): Unit = {

      connections.entrySet().asScala.map(_.getValue).foreach { connection =>
        connection.tempBuckets.foreach { tempBucketName =>
          if(connection.clusterManager.hasBucket(tempBucketName)) {
            connection.clusterManager.removeBucket(tempBucketName)
            connection.clusterManager.removeUser(AuthDomain.LOCAL, tempBucketName)
          }
        }
        connection.cluster.disconnect()
        connection.env.shutdown()
      }
    }
  })
}

trait CouchbaseTestHelper extends MiscTestHelper with LazyLogging {
  import CouchbaseTestHelper._

  def couchbaseCfg: CouchbaseCfg

  def withCouchbase[A](f: CouchbaseConnection => A): A = {
    val connectCfg = ConnectCfg(couchbaseCfg)
    val connection = connections.computeIfAbsent(connectCfg, new JavaFunction[ConnectCfg,CouchbaseConnection] {
      override def apply(connectCfg: ConnectCfg): CouchbaseConnection = {
        openCouchbaseConnection(
          nodes = couchbaseCfg.nodes,
          username = couchbaseCfg.username,
          password = couchbaseCfg.password
        )
      }
    })
    f(connection)
  }

  def registerTempBucketName(tempBucketName: String) : Unit = {
    val connectCfg = ConnectCfg(couchbaseCfg)
    connections.computeIfPresent(connectCfg, new java.util.function.BiFunction[ConnectCfg, CouchbaseConnection, CouchbaseConnection] {
      override def apply(notused: ConnectCfg, connection: CouchbaseConnection): CouchbaseConnection =
        connection.copy(tempBuckets = tempBucketName :: connection.tempBuckets)
    })
  }

  def openCouchbaseConnection(
    nodes: List[String],
    username: String,
    password: String
  ) : CouchbaseConnection = {
    logger.info(s"Connecting to couchbase: nodes=${nodes.mkString(",")} username=$username password=${maskPassword(password)}")
    val env = DefaultCouchbaseEnvironment.builder()
      // this doesn't seem to do much
      .dnsSrvEnabled(false)

      .build()
    val cluster = CouchbaseCluster.create(env, nodes.asJava)
    val clusterManager = cluster.clusterManager(username, password)
    CouchbaseConnection(
      env = env,
      cluster = cluster,
      clusterManager = clusterManager,
      tempBuckets = Nil
    )
  }

  def withTempBucket[A](tempBucketName: String)(f: => A) : A = {
    withCouchbase { implicit connection =>
      if(connection.clusterManager.hasBucket(tempBucketName)) {
        truncateBucket(tempBucketName)
      } else {
        createBucket(tempBucketName)
        createBucketUser(tempBucketName)
        createBucketPrimaryIndex(tempBucketName)
        registerTempBucketName(tempBucketName)
      }
    }

    f
  }


  def createBucketUser(bucketName: String)(implicit connection: CouchbaseConnection) : Unit = {
    // must add temp bucket user with same name as bucket with password that is used to open bucket
    logger.info(s"Creating bucket user: $bucketName")
    connection.clusterManager.upsertUser(
      AuthDomain.LOCAL,
      bucketName,
      UserSettings.build()
        .name(bucketName)
        .password(bucketName)
        .roles(List(
          new UserRole("bucket_admin",bucketName),
          new UserRole("bucket_full_access",bucketName)
        ).asJava)
    )
  }


  def withBucket[A](bucketName: String, password: String)(f: Bucket => A)(implicit connection: CouchbaseConnection) : A = {
    val bucket = connection.cluster.openBucket(bucketName, password)
    try {
      f(bucket)
    } finally {
      bucket.close()
    }
  }

  def withBucket[A](bucketName: String, password: String, timeout: Duration)(f: Bucket => A)(implicit connection: CouchbaseConnection) : A = {
    val bucket = connection.cluster.openBucket(bucketName, password, timeout.toMillis, TimeUnit.MILLISECONDS)
    try {
      f(bucket)
    } finally {
      bucket.close()
    }
  }

  def createBucketPrimaryIndex(bucketName: String)(implicit connection: CouchbaseConnection) : Unit = {
    // must open bucket to run create primary index query against it
    // note: need more time since bucket is new
    withBucket(bucketName, bucketName, 30.seconds) { bucket =>
      logger.info(s"Creating bucket primary index: $bucketName")
      val result = bucket.query(N1qlQuery.simple(s"CREATE PRIMARY INDEX `IDX_$bucketName` ON `$bucketName`"))
      if(result.errors().asScala.nonEmpty) {
        throw new RuntimeException(s"Failed to create primary index for bucket $bucketName:" + result.errors.toString)
      }
    }
  }

  def createBucket(bucketName: String)(implicit connection: CouchbaseConnection) : Unit = {
    val bucketSettings = new DefaultBucketSettings.Builder()
      .`type`(BucketType.COUCHBASE)
      .name(bucketName)
      .quota(100)
      .enableFlush(true)
      .build()

    logger.info(s"Creating bucket: $bucketName")
    connection.clusterManager.insertBucket(bucketSettings)
  }
  def truncateBucket(bucketName: String)(implicit connection: CouchbaseConnection) : Unit = {
    withBucket(bucketName, bucketName)(_.bucketManager().flush())
  }
}
