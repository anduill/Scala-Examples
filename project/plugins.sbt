// creating credentials example
//credentials += {
//  Option(System.getenv("ARTIFACTORY_CREDENTIALS")) flatMap { encoded =>
//    def decode(e : String) = {
//      ...decode some stuff
//    }
//    val decoded = scala.util.Try(decode(encoded))
//    decoded recover { case ex => ex.printStackTrace() }
//    decoded.toOption map { case (user, pass) =>
//      Credentials("Artifactory Realm", "company.jfrog.io", user, pass)
//    }
//  } getOrElse Credentials(Path.userHome / ".sbt" / ".credentials")
//}

addSbtPlugin("io.get-coursier" % "sbt-coursier" % "1.0.3")

addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.14.7")

addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.5.1")

addSbtPlugin("io.spray" % "sbt-revolver" % "0.9.1")

