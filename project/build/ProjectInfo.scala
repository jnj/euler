import sbt._

class EulerProject(info: ProjectInfo) extends DefaultProject(info)
{
  val scalaTest = "org.scalatest" % "scalatest" % "0.9.5" 
}
