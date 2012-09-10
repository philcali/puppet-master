name := "puppet-master"

organization := "com.github.philcali"

version := "0.1.0"

scalaVersion := "2.9.2"

scalacOptions ++= Seq("-deprecation", "-unchecked")

crossScalaVersions := Seq("2.9.2", "2.9.1", "2.8.2", "2.8.1")

libraryDependencies ++= Seq(
  "com.github.philcali" %% "css-query-core" % "0.1.0",
  "net.databinder.dispatch" %% "core" % "0.9.0",
  "org.ccil.cowan.tagsoup" % "tagsoup" % "1.2.1",
  "com.github.philcali" %% "lmxml-core" % "0.1.2",
  "org.scalatest" %% "scalatest" % "1.8" % "test"
)
