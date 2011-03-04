package mongo

import net.liftweb.json.JsonAST._
import com.mongodb.BasicDBObject

case class IndexDef(obj: BasicDBObject, params: Option[BasicDBObject])
case class CollectionDef(name: String, docs: List[BasicDBObject], indexes: List[IndexDef])
  
object Seed {
  import Implicits._ // JObject%asDBObject
  
  val asJObject: JValue => Option[JObject] = {
    case o: JObject => Some(o)
    case _ => None
  }
    
  val transformObj: JValue => Option[BasicDBObject] = asJObject(_) map(_ asDBObject)
  
  def indexDef(obj: JValue): Option[Either[String, IndexDef]] = obj match {
    case JNothing => None
    case JArray((index: JObject) :: ps) =>
      val params = ps match {
        case (p: JObject) :: Nil => Some(p asDBObject) // TODO option appropriate for IndexDef?
        case _ => None
      }
      Some(Right(IndexDef(index asDBObject, params)))
    case _ => Some(Left("Invalid index definition."))
  }

  /* it'd be nice to not have to yield Left(l :: Nil) */
  
  def collection(o: JObject) =
    o \ "name" match {
      case JField(_, JString(name)) => Right(name)
      case x => Left("Collection has an invalid or missing 'name' key." :: Nil)
    }
    
  /* Extract all JObjects */
  def docs(o: JObject) =
    o \ "docs" match {
      case JField(_, JArray(fields)) => Right(fields map (transformObj) flatMap (x => x))
      case _ => Left("No documents found in collection!" :: Nil)
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
