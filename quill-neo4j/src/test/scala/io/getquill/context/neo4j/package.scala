package io.getquill.context

import io.getquill._

package object neo4j {
  lazy val mirrorContext = new Neo4jMirrorContext with TestEntities
}
