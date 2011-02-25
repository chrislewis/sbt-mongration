import sbt._

class Project(info: ProjectInfo) extends PluginProject(info) {
  val mongo = "org.mongodb" % "mongo-java-driver" % "1.3"
  val lift_json = "net.liftweb" %% "lift-json" % "2.2"
}
