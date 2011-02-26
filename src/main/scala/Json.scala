package mongration.json

import net.liftweb.json._
import JsonAST._
import com.mongodb.BasicDBObject

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
  
  val conversionMap: Map[String, Any => Any] = Map(
    "$date" -> (d => new java.util.Date(d.asInstanceOf[BigInt].longValue))
  )
  
  def toJava[A](sl: List[A]) =
    sl.foldLeft(new java.util.ArrayList[A](sl.size)) { (jl, i) =>
      jl.add(i)
      jl
    }
  
  /** JArray => List[Any] */
  def parseArray(jarr: JArray): List[Any] = {
    def _parseArray(values: List[JValue], acc: List[Any]): List[Any] = values match {
      case Nil => acc
      case JScalar(value) :: xs => _parseArray(xs, value :: acc) 
      case JArray(values) :: xs => _parseArray(xs, _parseArray(values, Nil) :: acc)
      case (o @ JObject(_)) :: xs => _parseArray(xs, parseMongoDBObject(o) :: acc)
      case _ => _parseArray(values, acc) // JNull, JNothing
    }
    _parseArray(jarr.arr, Nil)
  }
  
  /* Convert a single-field object into some type. Useful for mapping mongo extended types. */
  def transformMongoType(fields: List[JField]): Option[Any] = fields match {
    case JField(name, JScalar(value)) :: Nil => conversionMap.get(name).map(_(value))
    case _ => None
  }
  
  /** JObject => BasicDBObject */
  def parseMongoDBObject(obj: JObject) = {
    def _parseObject(fields: List[JField], obj: BasicDBObject): BasicDBObject = fields match {
      case Nil => obj
      case JField(name, JScalar(value)) :: tail =>
        obj.put(name, value);
        _parseObject(tail, obj)
        
      case JField(name, a @ JArray(_)) :: tail =>
        val arr = toJava(parseArray(a))
        obj.put(name, arr)
        _parseObject(tail, obj)
        
      case JField(name, JObject(fields)) :: tail =>
        obj.put(name, transformMongoType(fields) getOrElse {
          _parseObject(fields, new BasicDBObject)
        })
        _parseObject(tail, obj)
      
      case JField(name, _) :: tail =>
        obj.put(name, null)
        _parseObject(tail, obj) // JNull, JNothing
    }
    _parseObject(obj.obj, new BasicDBObject)
  }
}
