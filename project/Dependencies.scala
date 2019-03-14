import sbt._

object Dependencies {
  object Versions {
    val refined = "0.9.3"
    val cats = "1.4.0"
  }
  
  lazy val refined = Seq(
    "eu.timepit" %% "refined" % Versions.refined, 
    "eu.timepit" %% "refined-cats" % Versions.refined
  )

  lazy val cats = Seq(
    "org.typelevel" %% "cats-core" % Versions.cats
  )
  
  lazy val scalaAsync = Seq(
    "org.scala-lang.modules" %% "scala-async" % "0.9.7"
  )
  
  lazy val scalaStm = Seq(
    "org.scala-stm" %% "scala-stm" % "0.8"
  )
}
