package mongo.json

import net.liftweb.json.JsonAST._

/** General extractor for scalar JValue types.*/
object JScalar {
  def unapply(jv: JValue) = jv match {
    case JString(value) => Some(value)
    case JDouble(value) => Some(value)
    case JInt(value) => Some(value)
    case JBool(value) => Some(value)
    case _ => None
  }
}

object Json {
  import com.mongodb.BasicDBObject
  import compat.collections.sj.Implicits._
  
  val conversionMap: Map[String, Any => Any] = Map(
    "$date" -> (d => new java.util.Date(d.toString.toLong))
  )
  
  /** JArray => List[Any] */
  def parseArray[A](f: JObject => A)(jarr: JArray): List[Any] = {
    def _parseArray(values: List[JValue], acc: List[Any]): List[Any] = values match {
      case Nil => acc
      case JScalar(value) :: xs => _parseArray(xs, value :: acc) 
      case JArray(values) :: xs => _parseArray(xs, _parseArray(values, Nil) :: acc)
      case (o @ JObject(_)) :: xs => _parseArray(xs, f(o) :: acc)
      case _ => _parseArray(values, acc) // JNull, JNothing
    }
    _parseArray(jarr.arr, Nil)
  }
  
  /** Convert a single-field object into some type. Useful for mapping mongo extended types. */
  def transformMongoType(fields: List[JField]): Option[Any] = fields match {
    case JField(name, JScalar(value)) :: Nil => conversionMap.get(name).map(_(value))
    case _ => None
  }
  
  /** JObject => BasicDBObject */
  def parseMongoDBObject(obj: JObject): BasicDBObject = {
    def _parseObject(fields: List[JField], obj: BasicDBObject): BasicDBObject = fields match {
      case Nil => obj
      case JField(name, JScalar(value)) :: tail =>
        obj.put(name, value);
        _parseObject(tail, obj)
        
      case JField(name, a @ JArray(_)) :: tail =>
        obj.put(name, parseArray(parseMongoDBObject)(a).asJava)
        _parseObject(tail, obj)
        
      case JField(name, JObject(flds)) :: tail =>
        val procd = transformMongoType(flds) getOrElse { _parseObject(flds, new BasicDBObject) }
        obj.put(name, procd)
        _parseObject(tail, obj)
      
      case JField(name, _) :: tail =>
        obj.put(name, null)
        _parseObject(tail, obj) // JNull, JNothing
    }
    _parseObject(obj.obj, new BasicDBObject)
  }
  
}
