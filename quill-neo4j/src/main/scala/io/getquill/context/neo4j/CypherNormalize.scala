package io.getquill.context.neo4j

import io.getquill.ast.Ast
import io.getquill.norm.FlattenOptionOperation
import io.getquill.norm.Normalize
import io.getquill.norm.RenameProperties

object CypherNormalize {

  def apply(ast: Ast) =
    normalize(ast)

  private[this] val normalize =
    (identity[Ast] _)
      .andThen(FlattenOptionOperation.apply _)
      .andThen(Normalize.apply _)
      .andThen(RenameProperties.apply _)
}