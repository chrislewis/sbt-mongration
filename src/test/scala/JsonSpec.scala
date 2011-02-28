package mongo

import json._
import org.specs._
import net.liftweb.json.JsonAST._

class JsonSpec extends Specification {
  
  "a scalar JSON extractor" should {
    "extract an Int from a JInt" in {
      val JScalar(scalar) = JInt(42)
      scalar must_== 42
    }
    "extract an String from a JString" in {
      val JScalar(scalar) = JString("Don't be afraid of ideas that are not your own")
      scalar must_== "Don't be afraid of ideas that are not your own"
    }
    "extract an Boolean from a JBool" in {
      val JScalar(scalar) = JBool(true)
      scalar must_== true
    }
    
    "not extract JArray" in {
      JScalar.unapply(JArray(List(JInt(1)))) must_== None
    }
    "not extract JField" in {
      JScalar.unapply(JField("name", JString("chris"))) must_== None
    }
    "not extract JObject" in {
      JScalar.unapply(JObject(List(JField("name", JString("chris"))))) must_== None
    }
  }
  
  "Json.parseArray" should {
    
    val parseArray = Json.parseArray(x => x)_
    val int_list = parseArray(JArray(List(JInt(1))))
    val twoD_mixed = parseArray(JArray(List(JInt(1), JArray(List(JString("hello"))))))
    
    "parse a list of JInts" in {
      int_list must_== List(1)
    }
    "parse a nested and mixed array" in {
      twoD_mixed must_== List(1, List("hello")).reverse
    }
  }
}
