package mongration

import json._
import Json.{parseMongoDBObject => parseDBObject}
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
  
  lazy val mongoSeed = task {
    FileUtilities.readString(seed.asFile, log) match {
      case Left(err) => log.error(err)
      case Right(value) =>
        JsonParser.parse(value) match {
          case JArray(values) =>
            val persist = persistCollection(mongo_db)_
            values
              .map(collectionDefs)
              .foreach(persist)
        }
    }
    
    None
  } describedAs("Populate the database with seed data.")
  
  def persistCollection(db: DB)(c: CollectionMeta) = c match {
    case CollectionMeta(name, docs, indexes) =>
      val col = db.getCollection(name)
      log.info(" - {%s} Persisting %d document(s) " format(name, docs.size))
      docs.foreach(col.insert)
      indexes.foreach {
        case (idef, Some(iargs)) => col.ensureIndex(idef, iargs)
        case (idef, None) => col.ensureIndex(idef)
      }
  }
  
  lazy val mongoReset = task {
    mongoSeed.run
    None
  } dependsOn(mongoDrop)
  
}


object Mongration {
  
  type Host = (String, Int)
  type Auth = (String, String)
  /* An index definition. */
  type IndexDef = (BasicDBObject, Option[BasicDBObject])
  
  case class CollectionMeta(name: String, docs: List[BasicDBObject], indexes: List[IndexDef])
  
  def configure(host: Host, database: String, auth: Auth) = (host, auth) match {
    case ((host, port), (user, password)) =>
      val m = new Mongo(host, port)
      val db = m.getDB(database)
      db.authenticate(user, password.toArray)
      (m, db)
  }
  
  /* Convert a JArray into an IndexDef. */
  def extractIndexDef(jv: JValue): IndexDef = jv match {
    case JArray((index: JObject) :: xs) => (parseDBObject(index), xs) match {
      case (obj, Nil) => (obj, None)
      case (obj, (params: JObject) :: Nil) => (obj, Some(parseDBObject(params)))
    }
  }
  
  /* Extract index definitions expected from the "indexes" key. */
  def extractIndexDefs(jv: JValue): List[IndexDef] = jv match {
    case JField(_, JArray(indexDefs)) => indexDefs map extractIndexDef
  }
  
  /* Parse a JSON object into Tuple2 of a collection name and its list of documents (preserved as JSON objects). */
  def collectionDefs(obj: JValue) = obj match {
    /* Can't deconstruct a list because order shouldn't matter. */
    case o: JObject => (o \ "name", o \ "docs", o \ "indexes") match {
      case (JField(_, JString(collection)), JField(_, JArray(jdocs)), indexes) =>
        CollectionMeta(collection,
          jdocs map transformObj,
          extractIndexDefs(indexes))
    }
  }
  
  /* JObject => BasicDBObject */
  def transformObj(obj: JValue) = obj match {
    case o: JObject => parseDBObject(o)
  }
}
