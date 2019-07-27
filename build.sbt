import Dependencies._

name := "concurrent_scala"

version := "0.1"

scalaVersion := "2.11.8"

resolvers ++= Seq(
  "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots", 
  "Sonatype OSS Releases" at "https://oss.sonatype.org/content/repositories/releases"
)

libraryDependencies ++= refined ++ cats ++ scalaAsync ++ scalaStm ++ reactors