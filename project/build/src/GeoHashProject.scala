import sbt._

class GeoHashProject(info: ProjectInfo) extends DefaultProject(info)
{
  val scalatest = "org.scalatest" % "scalatest" % "1.2" % "provided->default"
}
