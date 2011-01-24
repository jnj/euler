import sbt._

class EulerProject(info: ProjectInfo) extends DefaultProject(info) {
  val scalaz = "com.googlecode.scalaz" % "scalaz-core_2.8.0" % "5.0"
  val scalaTest = "org.scalatest" % "scalatest" % "1.2"
}
