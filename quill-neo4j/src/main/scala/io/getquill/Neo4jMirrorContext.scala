package io.getquill

import io.getquill.context.neo4j.CypherIdiom

class Neo4jMirrorContext[Naming <: NamingStrategy]
  extends MirrorContext[CypherIdiom, Naming] {
}