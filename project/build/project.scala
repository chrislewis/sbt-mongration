import sbt._

class Project(info: ProjectInfo) extends PluginProject(info)
  with gh.Issues {
  
  val mongo = "org.mongodb" % "mongo-java-driver" % "1.3"
  val lift_json = "net.liftweb" %% "lift-json" % "2.2"
  val java_utils = "org.scala-tools.javautils" %% "javautils" % "0.2"
  val specs = "org.scala-tools.testing" %% "specs" % "1.6.1" % "test"
  
  // gh issues
  def ghCredentials = gh.LocalGhCreds(log)
  def ghRepository = ("chrislewis", "sbt-mongration")
  
  val r_specs = "specs repo" at "http://specs.googlecode.com/svn/maven2/"
  val r_lessis = "less is repo" at "http://repo.lessis.me"
}
