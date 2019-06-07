package com.example

import java.util
import java.util.{HashMap, Map}

import com.google.common.collect.ImmutableList
import org.apache.hadoop.conf.Configuration
import org.apache.hadoop.filecache.DistributedCache
import org.apache.hadoop.fs.Path
import org.apache.hadoop.mapreduce.filecache.ClientDistributedCacheManager
import org.apache.hadoop.mapreduce.v2.util.MRApps
import org.apache.hadoop.security.Credentials
import org.apache.hadoop.yarn.api.ApplicationConstants
import org.apache.hadoop.yarn.api.records._
import org.apache.hadoop.yarn.client.api.AMRMClient.ContainerRequest
import org.apache.hadoop.yarn.client.api.YarnClient
import org.apache.hadoop.yarn.client.api.async.AMRMClientAsync
import org.apache.hadoop.yarn.conf.YarnConfiguration
import org.apache.hadoop.yarn.util.Records

import collection.JavaConverters._

object YarnSubmission extends App {
  println("hello there!!")
  val localConf = new Configuration()
  localConf.addResource(new Path("file:///Users/david.garcia/development/repos/hadoop/hadoop-dist/target/hadoop-2.8.3/etc/hadoop/yarn-site.xml"))
  localConf.addResource(new Path("file:///Users/david.garcia/development/repos/hadoop/hadoop-dist/target/hadoop-2.8.3/etc/hadoop/core-site.xml"))
  localConf.addResource(new Path("file:///Users/david.garcia/development/repos/hadoop/hadoop-dist/target/hadoop-2.8.3/etc/hadoop/hdfs-site.xml"))

  val yarnClient = YarnClient.createYarnClient()
  yarnClient.init(localConf)
  yarnClient.start()
  val yarnApplication = yarnClient.createApplication()



  val appContext = Records.newRecord(classOf[ApplicationSubmissionContext])
  appContext.setResource(Resource.newInstance(1024, 1))
  val priority = Records.newRecord(classOf[Priority])
  priority.setPriority(-1)
  appContext.setPriority(priority)
//  val appId = ApplicationId.fromString("application_1549911951234_0003")//yarnApplication.getNewApplicationResponse.getApplicationId
  val appId = yarnApplication.getNewApplicationResponse.getApplicationId
  println(appId)
  appContext.setApplicationId(appId)
  appContext.setApplicationName("TestShapeless-finish-correctly")
  appContext.setApplicationType("Bulk Launcher")
  appContext.setMaxAppAttempts(1)

  DistributedCache.addLocalArchives(localConf, "file:///Users/david.garcia/development/repos/Scala-Examples/target/scala-2.11/example-project.jar")
  DistributedCache.addLocalFiles(localConf, "file:///Users/david.garcia/development/repos/Scala-Examples/target/scala-2.11/example-project.jar")
  val localResources: util.Map[String, LocalResource] = new util.HashMap[String, LocalResource] //TODO: Add Resources as local Resources
  ClientDistributedCacheManager.determineTimestampsAndCacheVisibilities(localConf)
  MRApps.setupDistributedCache(localConf, localResources)

  val cmd: util.ArrayList[String] = new util.ArrayList[String]()
  cmd.add("/Users/david.garcia/development/repos/hadoop/hadoop-dist/target/hadoop-2.8.3/bin/hadoop")
  cmd.add("jar")
  cmd.add("/Users/david.garcia/development/repos/Scala-Examples/target/scala-2.11/example-project.jar")
  cmd.add("com.example.TestShapeless")
  cmd.add("1>" + ApplicationConstants.LOG_DIR_EXPANSION_VAR + Path.SEPARATOR + ApplicationConstants.STDOUT)
  cmd.add("2>" + ApplicationConstants.LOG_DIR_EXPANSION_VAR + Path.SEPARATOR + ApplicationConstants.STDERR)

  val mergedCommand = new StringBuilder
  cmd.asScala.toList.foreach({arg =>
    mergedCommand.append(arg).append(" ")
  })
  val yarn_args = ImmutableList.of(mergedCommand.toString())

  val amContainer = ContainerLaunchContext.newInstance(localResources, new util.HashMap[String, String](), yarn_args,null,null,null)//Records.newRecord(classOf[ContainerLaunchContext])
  amContainer.setCommands(yarn_args)
  appContext.setAMContainerSpec(amContainer)
  appContext.setCancelTokensWhenComplete(true)

  //Tokens
  val creds = new Credentials()
  val tokenReviewer = localConf.get(YarnConfiguration.RM_PRINCIPAL)//doesn't seem to matter

  yarnClient.submitApplication(appContext)

  val appReport : ApplicationReport = yarnClient.getApplicationReport(appId)
  val diags = appReport.getDiagnostics
  val consoleUrl = appReport.getTrackingUrl
  println(s"Report: $appReport")
  println(s"ConsoleURL: $consoleUrl")
  val resource = Resource.newInstance(512, 1)
  val contRequest = new ContainerRequest(resource,null,null, priority)
  val allocListener = AMRMClientAsync.createAMRMClientAsync(1000, new AMRMClientAsync.CallbackHandler() {
    override def onContainersCompleted(list: util.List[ContainerStatus]): Unit = {}

    override def onContainersAllocated(list: util.List[Container]): Unit = {}

    override def onShutdownRequest(): Unit = {
      println("Resource manager requested a shutdown")
    }

    override def onNodesUpdated(list: util.List[NodeReport]): Unit = {}

    override def getProgress: Float = {0.5f}

    override def onError(throwable: Throwable): Unit = {
      println("Received asynchronous error")
      throwable.printStackTrace()
    }
  })
  allocListener.init(localConf)

}
