package io.getquill.context.neo4j

import io.getquill.ast._
import io.getquill.util.Messages.fail

sealed trait FromContext
case class NodeContext(node: Entity, label: String) extends FromContext

case class CypherQuery(
  entity: FromContext,
  filter: Option[Ast],
  select: List[Ast]
)

object CypherQuery {

  def apply(q: Query): CypherQuery =
    q match {
      case Map(q: Query, Ident(label), p) =>
        apply(q, select(p), label)
      case other =>
        apply(q, List(), "x")
    }

  private def apply(q: Query, select: List[Ast], label: String): CypherQuery =
    q match {
      case Filter(q: Query, x, p) =>
        apply(q, Some(p), select, label)
      case other =>
        apply(q, None, select, label)
    }

  private def apply(q: Query, filter: Option[Ast], select: List[Ast], label: String): CypherQuery =
    q match {
      case q: Entity =>
        new CypherQuery(NodeContext(q, label), filter, select)
      case q =>
        fail(s"Invalid Cypher querys: $q")
    }

  private def select(ast: Ast): List[Ast] =
    ast match {
      case Tuple(values) => values.flatMap(select)
      case p: Property   => List(p)
      case i: Ident      => List()
      case l: Lift       => List(l)
      case other         => fail(s"Cypher supports only properties as select elements. Found: $other")
    }
}
