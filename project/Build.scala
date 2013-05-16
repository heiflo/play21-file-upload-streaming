import sbt._
import Keys._
import play.Project._

object ApplicationBuild extends Build {

  val appName         = "play21-file-upload-streaming"
  val appVersion      = "1.0-SNAPSHOT"

  val appDependencies = Seq()

  scalaVersion := "2.10.1"

 scalacOptions ++= Seq("-deprecation", "-encoding", "UTF-8", "-feature", "-target:jvm-1.6", "-unchecked",
    "-Ywarn-adapted-args", "-Ywarn-value-discard", "-Xlint")

  val main = play.Project(appName, appVersion, appDependencies).settings(
    // Add your own project settings here      
  )

}
