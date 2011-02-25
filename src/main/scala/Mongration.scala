package mongration

import json._
import sbt._
import com.mongodb._
import net.liftweb.json._
import JsonAST._

trait Mongration extends Project {
  import Mongration._
  def configure: (Mongo, DB)
  lazy val (mongo_con, mongo_db) = configure
  
  def seed = "src" / "test" / "resources" / "seed.json"
  
  lazy val mongoDrop = task {
    mongo_db.dropDatabase()
    None
  } describedAs("[!] Destroy the current database. Use with caution.")
  
  def persistCollection(col: (String, List[BasicDBObject])) = col match {
    case (name, docs) =>
      val col = mongo_db.getCollection(name)
      log.info(" - {%s} Persisting %d document(s) " format(name, docs.size))
      docs.foreach(col.insert)
  }
  
  lazy val mongoSeed = task {
    FileUtilities.readString(seed.asFile, log) match {
      case Left(err) => log.error(err)
      case Right(value) => JsonParser.parse(value) match {
        case JArray(values) =>
          values
            .map(baseCollections)
            .map(cookObjs)
            .foreach(persistCollection)
      }
    }
    
    None
  } describedAs("Populate the database with seed data.")
  
  lazy val mongoReset = task {
    mongoSeed.run
    None
  } dependsOn(mongoDrop)
  
}

object Mongration {
  
  type Host = (String, Int)
  type Auth = (String, String)
  
  def configure(host: Host, database: String, auth: Auth) = (host, auth) match {
    case ((host, port), (user, password)) =>
      val m = new Mongo(host, port)
      val db = m.getDB(database)
      db.authenticate(user, password.toArray)
      (m, db)
  }
  
  /* JObject => BasicDBObject */
  def transformObj(obj: JValue) = obj match {
    case o @ JObject(_) => Json.parseMongoDBObject(o)
  }
  
  /* Parse a JSON object into Tuple2 of a collection name and its list of documents (preserved as JSON objects). */
  def baseCollections(obj: JValue): (String, List[JValue]) = obj match {
    case JObject(JField("collection", JString(collection)) :: JField("docs", JArray(jdocs)) :: Nil) =>
      (collection, jdocs)
  }
  
  /* Produce a Tuple2 of a collection and its list of documents. */
  def cook[A](f: JValue => A)(raw: (String, List[JValue])): (String, List[A]) = raw match {
    case (collection, docs) => (collection, docs.map(f))
  }
  
  /* Produce a Tuple2 of a collection and its list of documents (as BasicDBObjects). */
  val cookObjs = cook(transformObj)_
}
