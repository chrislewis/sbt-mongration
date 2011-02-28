package mongo

import net.liftweb.json.JsonAST._

class MongoJObject(obj: JObject) {
  def asDBObject = json.Json.parseMongoDBObject(obj)
}

object Implicits {
  implicit def JObject2Mongo(j: JObject) = new MongoJObject(j)
}
