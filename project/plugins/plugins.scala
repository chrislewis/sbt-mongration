import sbt._

class Plugins(info: ProjectInfo) extends PluginDefinition(info) {
  val ghIssues = "me.lessis" % "sbt-gh-issues" % "0.1.0"
  val r_lessis = "less is repo" at "http://repo.lessis.me"
}
