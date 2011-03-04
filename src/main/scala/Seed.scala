package mongo

import net.liftweb.json.JsonAST._
import com.mongodb.BasicDBObject

case class IndexDef(obj: BasicDBObject, params: Option[BasicDBObject])
case class CollectionDef(name: String, docs: List[BasicDBObject], indexes: List[IndexDef])
  
object Seed {
  import Implicits._ // JObject%asDBObject
  
  def transformObj(obj: JValue) = obj match {
    case o: JObject => Some(o.asDBObject)
    case _ => None
  }
  
  def indexDef(obj: JValue): Option[Either[String, IndexDef]] = obj match {
    case JNothing => None
    case JArray((index: JObject) :: p) =>
      val i = p match {
        case (params: JObject) :: Nil => Some(params asDBObject) // TODO option appropriate for IndexDef?
        case _ => None
      }
      Some(Right(IndexDef(index asDBObject, i)))
    case _ => Some(Left("Invalid index definition."))
  }

  /* TODO it'd be nice to not have to yield Left(l :: Nil) */
  
  def collection(o: JObject) =
    o \ "name" match {
      case JField(_, JString(name)) => Right(name)
      case x => Left("Collection has an invalid or missing 'name' key." :: Nil)
    }
    
  /* Extract all JObjects */
  def docs(o: JObject) =
    o \ "docs" match {
      case JField(_, JArray(fields)) => Right(fields map (transformObj) flatMap (x => x))
      case _ => Left("no docs!" :: Nil)
    }
  
  def indexes(o: JObject): Either[List[String], List[IndexDef]] = 
    o \ "indexes" match {
      case JField(_, JArray(fields)) => fields flatMap (indexDef) lefts
      case _ => Right(Nil)
    }
  
  def collectionDef(o: JObject): Either[List[String], CollectionDef] =
    for {
      c <- collection(o)
      d <- docs(o)
      i <- indexes(o)
    } yield CollectionDef(c, d, i)
}
