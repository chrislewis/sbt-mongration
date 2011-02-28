package mongo

object Seed {
  import Implicits._
  import net.liftweb.json.JsonAST._
  import com.mongodb.BasicDBObject
  
  /* A mongo collection index definition. */
  case class IndexDef(obj: BasicDBObject, params: Option[BasicDBObject])
  case class CollectionDef(name: String, docs: List[BasicDBObject], indexes: List[IndexDef])
  
  /** Convert a JArray into an Either[String, IndexDef]. */
  def indexDef(where: String)(jv: JValue): Either[String, IndexDef] = jv match {
    case JArray((index: JObject) :: xs) =>
      val iparams = xs match {
        case (params: JObject) :: Nil => Some(params.asDBObject)
        case _ => None
      }
      Right(IndexDef(index.asDBObject, iparams))
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
  
  /* Concat a value with a list of values of the same type. */
  private def favorLeft[A, B](a: Either[List[A], List[B]], e: Either[A, B]) = (a, e) match {
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
