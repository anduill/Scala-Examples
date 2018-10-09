import sbt._

object Dependencies {

  object Versions {
    val scalaLogging          = "3.9.0"
    val shapeless             = "2.3.2"
    val storehausCache        = "0.15.0"
    val jsonPath              = "0.6.9"
    val scalaTest             = "3.0.5"
    val scalaMock             = "3.6.0"
  }

  object Libraries {
    val scalaLogging                 = "com.typesafe.scala-logging"  %% "scala-logging"                    % Versions.scalaLogging
    val scalaMock                    = "org.scalamock"               %% "scalamock-scalatest-support"      % Versions.scalaMock             % "test"
    val scalaTest                    = "org.scalatest"               %% "scalatest"                        % Versions.scalaTest             % "test"
    val shapeless                    = "com.chuusai"                 %% "shapeless"                        % Versions.shapeless
  }

  lazy val dependencies = Seq(
    Libraries.scalaLogging,
    Libraries.scalaMock,
    Libraries.scalaTest,
    Libraries.shapeless
  )
}
