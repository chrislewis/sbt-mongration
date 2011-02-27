package mongration

case class Auth(val user: String, val password: String)
case class Configurator(val host: String, val port: Int, val database: String, val auth: Option[Auth]) {
  def as(auth: Auth) = Configurator(host, port, database, Some(auth))
}

object Configurator {
  /* 2.8 named params will ease this */
  def apply(database: String): Configurator = Configurator("localhost", 27017, database, None)
  def apply(host: String, database: String): Configurator = Configurator(host, 27017, database, None)
}
