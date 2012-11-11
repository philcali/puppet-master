import sbt._
import Keys._

object PuppetBuild extends Build {
  val generalSettings = Defaults.defaultSettings ++ Seq(
    organization := "com.github.philcali",
    version := "0.1.0",
    scalaVersion := "2.9.2",
    scalacOptions ++= Seq("-deprecation", "-unchecked")
  )

  lazy val root = Project(
    "puppet-master",
    file("."),
    settings = generalSettings
  ) aggregate (core, app)

  lazy val core = Project(
    "puppet-master-core",
    file("core"),
    settings = generalSettings ++ Seq(
      crossScalaVersions := Seq("2.9.2", "2.9.1", "2.8.2", "2.8.1"),
      libraryDependencies <++= (scalaVersion) ( sv => Seq(
        "com.github.philcali" %% "css-query-core" % "0.1.0",
        "org.scala-lang" % "jline" % sv,
        "net.databinder.dispatch" %% "dispatch-core" % "0.9.4",
        "org.ccil.cowan.tagsoup" % "tagsoup" % "1.2.1",
        "com.github.philcali" %% "lmxml-core" % "0.1.2",
        "org.scalatest" %% "scalatest" % "1.8" % "test",
        "net.databinder" %% "unfiltered-netty-server" % "0.6.4" % "test",
        "net.databinder" %% "unfiltered-netty-uploads" % "0.6.4" % "test"
      ))
    )
  )

  lazy val app = Project(
    "puppet-master-app",
    file("app"),
    settings = generalSettings ++ Seq(
      libraryDependencies <++= (sbtVersion) { sv => Seq(
        "com.github.philcali" %% "lmxml-json" % "0.1.2",
        "org.slf4j" % "slf4j-simple" % "1.7.1",
        "org.scala-sbt" % "launcher-interface" % sv % "provided"
      ) }
    )
  ) dependsOn core
}
