package mongo

import net.liftweb.json.JsonAST._

class MongoJObject(obj: JObject) {
  def asDBObject = json.Json.parseMongoDBObject(obj)
}

object Implicits {
  implicit def JObject2Mongo(j: JObject) = new MongoJObject(j)
  
  implicit def either2rich[L, R](e: Either[L, R]) = new RichEither(e)
  
  implicit def e2e[L, R](es: List[Either[L, R]]) = new Eithers(es)
}
