package mongration

import json._
import sbt._
import com.mongodb._
import net.liftweb.json._
import JsonAST._

trait Mongration extends Project {
  import Mongration._
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
        case (idef, Some(iargs)) => col.ensureIndex(idef, iargs)
        case (idef, None) => col.ensureIndex(idef)
      }
  }
  
}

object Mongration {
  /* A mongo collection index definition. */
  type IndexDef = (BasicDBObject, Option[BasicDBObject])
  
  case class CollectionDef(name: String, docs: List[BasicDBObject], indexes: List[IndexDef])
  
  /* pimpin */
  implicit def JObject2MongoJObject(j: JObject) = new MongoJObject(j)
  
  def configure(cfg: Configurator) = cfg match {
    case Configurator(host, port, database, auth) =>
      val m = new Mongo(host, port)
      val db = m.getDB(database)
      auth foreach { case Auth(u, p) => db.authenticate(u, p.toArray) }
      (m, db)
  }
  
  /** Convert a JArray into an Either[String, IndexDef]. */
  def indexDef(where: String)(jv: JValue): Either[String, IndexDef] = jv match {
    case JArray((index: JObject) :: xs) =>
      val iparams = xs match {
        case (params: JObject) :: Nil => Some(params.asDBObject)
        case _ => None
      }
      Right(index.asDBObject -> iparams)
    case _ => Left("Invalid index definition in %s!" format where)
  }
  
  /** Parse a JSON object into an Either[String, CollectionDef]. */
  def collectionDefs(obj: JValue): Either[String, CollectionDef] = obj match {
    case o: JObject => collectionDefs(o)
    case x => Left("Expected object but found %s!" format (x.getClass.getName))
  }
  
  def collectionDefs(o: JObject): Either[String, CollectionDef] = 
    /* Can't deconstruct a list because order shouldn't matter. */
    (o \ "name", o \ "docs", o \ "indexes") match {
      case (JField(_, JString(collection)), JField(_, JArray(jdocs)), indexes) =>
        /* Accumulate index definitions if provided. */
        val idefs = indexes match {
          case JField(_, JArray(indexes)) =>
            val init: Either[List[String], List[IndexDef]] = Right(Nil)
            indexes
              .map(indexDef(collection))
              .foldLeft(init)(favorLeft)
          case _ => Right(Nil)
        }
        /* Accumulate document objects. */
        val docs = collapseOpts(jdocs map transformObj)
        /* Yield the collection definition. */
        (idefs, docs) match {
          case (Right(i), Right(d)) => Right(CollectionDef(collection, d, i))
          case (Left(e1), Left(e2)) => Left((e2 :: e1) mkString ", ")
          case (Left(e), _) => Left(e mkString ", ")
          case (_, Left(e)) => Left(e)
        }
      case _ => Left("Failed to parse collection metadata!")
    }
  
  /** Concatenate a value with a list of values of the same type. */
  def favorLeft[A, B](a: Either[List[A], List[B]], e: Either[A, B]) = (a, e) match {
    case (Left(xs), Left(x)) => Left(x :: xs)
    case (Left(xs), _) => Left(xs)
    case (_, Left(x)) => Left(x :: Nil)
    case (Right(xs), Right(x)) => Right(x :: xs)
  }
  
  /* JObject => Option[BasicDBObject] */
  def transformObj(obj: JValue) = obj match {
    case o: JObject => Some(o.asDBObject)
    case _ => None
  }
  
  /* TODO extract + genericize */
  def collapseOpts[A](objs: List[Option[A]]): Either[String, List[A]] = {
    objs.foldLeft(Right(Nil).asInstanceOf[Either[String, List[A]]]) {
      case (b @ Left(_), _) => b
      case (_, None) => Left("Failed to extract object!")
      case (Right(objs), Some(o)) => Right(o :: objs)
    }
  }
  
}
