package io.getquill.context.neo4j

import io.getquill._

class CypherIdiomSpec extends Spec {

  import mirrorContext._

  case class Person(name: String, age: Int)

  "query" - {
    "map" in {

      val q = quote {
        query[Person]
      }

      mirrorContext.run(q).string mustEqual
        "MATCH (x:Person) RETURN x.name, x.age"
    }
    "filter" in {
      val q = quote {
        query[Person].filter(p => p.age > 18)
      }

      mirrorContext.run(q).string mustEqual
        "MATCH (p:Person) WHERE p.age > 18 RETURN p.name, p.age"
    }
    "filter and" in {
      val q = quote {
        query[Person]
          .filter(p => (p.age > 18 && p.age < 30))
          .filter(x => (x.age > 30 || x.age == 42))
      }

      mirrorContext.run(q).string mustEqual
        "MATCH (p:Person) WHERE ((p.age > 18) AND (p.age < 30)) AND ((p.age > 30) OR (p.age = 42)) RETURN p.name, p.age"
    }
  }
}