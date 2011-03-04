package mongo

import json._
import org.specs._
import net.liftweb.json.JsonAST._
import net.liftweb.json.JsonParser.parse

class SeedSpec extends Specification {
  val jdocs = parse("""
  {
    "indexes": [
      [{ "lastSeenAt" : "2d" }, { "min" : -500 , "max" : 500 }]
    ],
    "docs": [
      {
        "name": "Chris"
      }
    ]
  }
  """).asInstanceOf[JObject]
  
  val noi = parse("""
  {
    "docs": [
      {
        "name": "Chris"
      }
    ]
  }
  """).asInstanceOf[JObject]
  
  "docs" should {
    (Seed2.docs(jdocs) match {
      case Left(x) => ""
      case Right(x :: tail) => x.get("name")
    }) must_== "Chris"
  }
  
  "indexes" should {
    (Seed2.indexes(jdocs) match {
      case Left(x) => None
      case Right(IndexDef(o, Some(p)) :: tail) =>
        Some((o.get("lastSeenAt"), p.get("min"), p.get("max"))) 
    }) must_== Some(("2d", -500, 500))
  }
}
