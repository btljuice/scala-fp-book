import sbt._

object Dependencies {
  lazy val scalactic = "org.scalactic" %% "scalactic" % "3.2.2"
  lazy val scalaTest = "org.scalatest" %% "scalatest" % "3.2.2"
  lazy val scalaCheck = "org.scalacheck" %% "scalacheck" % "1.13.4"
  lazy val scalaTestCheck = "org.scalatestplus" %% "scalacheck-1-15" % "3.2.2.0"
}
