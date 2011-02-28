package mongo

import json._
import sbt._
import com.mongodb._
import net.liftweb.json._
import JsonAST._

trait Mongration extends Project {
  
  import Seed._
  
  def mongoConfig: Configurator
  lazy val (mongo_con, mongo_db) = configure(mongoConfig)
  
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
              // TODO fold into a single Either to avoid persiting anything if there were errors
              .map(_.fold(reportError, persist))
          case _ => log.error("Incomprehensible seed!")
        }
    }
    
    None
  } describedAs("Populate the database with seed data.")
  
  lazy val mongoReset = task {
    mongoSeed.run
    None
  } dependsOn(mongoDrop)
  
  def reportError(e: String) = log.error(e)
  
  def persistCollection(db: DB)(c: CollectionDef) = c match {
    case CollectionDef(name, docs, indexes) =>
      val col = db.getCollection(name)
      log.info(" - {%s} Persisting %d document(s) " format(name, docs.size))
      docs.foreach(col.insert)
      indexes.foreach {
        case IndexDef(idef, iargs) =>
          col.ensureIndex(idef, iargs.getOrElse(new BasicDBObject))
      }
  }
  
  def configure(cfg: Configurator) = cfg match {
    case Configurator(host, port, database, auth) =>
      val m = new Mongo(host, port)
      val db = m.getDB(database)
      auth foreach { case Auth(u, p) => db.authenticate(u, p.toArray) }
      (m, db)
  }
}
