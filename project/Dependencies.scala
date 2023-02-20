import sbt._

object Dependencies {
  val scalaTestVersion = "3.2.14"
  lazy val scalaTest = "org.scalatest" %% "scalatest" % scalaTestVersion
  lazy val scalaTestFunSuite = "org.scalatest" %% "scalatest-funsuite" % scalaTestVersion
}
