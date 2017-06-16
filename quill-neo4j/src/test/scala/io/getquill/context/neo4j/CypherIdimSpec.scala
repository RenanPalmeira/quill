package io.getquill.context.neo4j

import io.getquill._

class CypherIdiomSpec extends Spec {

  import mirrorContext._

  "query" - {
    "map" in {
      case class Person(name: String, age: Int)

      val q = quote {
        query[Person]
      }

      mirrorContext.run(q).string mustEqual
        "MATCH (x:Person) RETURN x.name, x.age"
    }
  }
}