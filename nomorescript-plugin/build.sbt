organization := "com.github.suzuki0keiichi"

name := "nomorescript-plugin"

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.9.2"

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-library" % "2.9.2" % "test",
  "org.scala-lang" % "scala-compiler" % "2.9.2",
  "junit" % "junit" % "4.8.2" % "test",
  "org.specs2" %% "specs2" % "1.12.1" % "test"
)

