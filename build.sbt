import sbtassembly.MergeStrategy
import Dependencies._

lazy val Name = "example-project"

lazy val basicSettings = Seq(
  organization            :=  "com.company",
  name                    :=  "example-project",
  organizationHomepage    :=  Some(new URL("http://www.company.com")),
  description             :=  "Example project for Scala",
  startYear               :=  Some(2016),
  scalaVersion            :=  "2.11.9",
  addCompilerPlugin("com.milessabin" % "si2712fix-plugin_2.11.8" % "1.2.0"),
  scalacOptions           :=  Seq(
    "-encoding", "utf8",
    "-unchecked",
    "-feature",
    "-language:existentials",
    "-language:higherKinds",
    "-language:implicitConversions",
    "-language:postfixOps",
    "-deprecation"),
  test in assembly := {},
  shellPrompt := { s => Project.extract(s).currentProject.id + " > " },
  assemblyOption in assembly := (assemblyOption in assembly).value.copy(cacheUnzip = false, cacheOutput = false),
  assemblyJarName in assembly := s"$Name.jar",
  assemblyMergeStrategy in assembly := {
    case "reference.conf"                                => MergeStrategy.concat
    case "rootdoc.txt"                                   => MergeStrategy.first
    case "application.conf"                              => MergeStrategy.concat
    case "logback.xml"                                   => MergeStrategy.first
    case "unwanted.txt"                                  => MergeStrategy.discard
    case PathList("org", "fusesource", "jansi", xs @ _*) => MergeStrategy.first
    case PathList("META-INF", "io.netty.versions.properties", xs @ _*) => MergeStrategy.last
    case PathList("META-INF", xs @ _*) => MergeStrategy.discard
    case PathList("javax", "servlet", xs @ _*)           => MergeStrategy.first
    case PathList("org", "slf4j", "impl", _*)            => MergeStrategy.first
    case PathList(ps @ _*) if ps.last endsWith ".html"   => MergeStrategy.first
    case x =>
      val oldStrategy = (assemblyMergeStrategy in assembly).value
      oldStrategy(x)
  }
)

val testSettings = Seq(
  parallelExecution in Test := false,
  logBuffered in Test := false,
  fork in Test := true
)


lazy val coursierSettings = Seq(
  coursierUseSbtCredentials := false
)


val buildSettings = basicSettings ++ testSettings ++ coursierSettings

lazy val project = Project(Name, file("."))
  .settings(buildSettings: _*)
  .settings(libraryDependencies ++= dependencies)

