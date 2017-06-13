package io.getquill.context.neo4j

import io.getquill.ast._
import io.getquill.NamingStrategy
import io.getquill.util.Messages.fail
import io.getquill.idiom.Idiom
import io.getquill.idiom.StatementInterpolator._
import io.getquill.idiom.Statement
import io.getquill.util.Interleave

object CypherIdiom extends CypherIdiom

trait CypherIdiom extends Idiom {

  override def liftingPlaceholder(idx: Int) = "?"

  override def prepareForProbing(string: String) = string

  override def emptyQuery = ""

  override def translate(ast: Ast)(implicit naming: NamingStrategy) = {
    val normalizedAst = CypherNormalize(ast)
    (normalizedAst, stmt"${normalizedAst.token}")
  }

  implicit def astTokenizer(implicit strategy: NamingStrategy, queryTokenizer: Tokenizer[Query]): Tokenizer[Ast] =
    Tokenizer[Ast] {
      case a: Query      => a.token
      case a: Operation  => a.token
      case a: Action     => a.token
      case a: Ident      => a.token
      case a: Property   => a.token
      case a: Value      => a.token
      case a: Function   => a.body.token
      case a: Infix      => a.token
      case a: Lift       => a.token
      case a: Assignment => a.token
      case a @ (
        _: Function | _: FunctionApply | _: Dynamic | _: OptionOperation | _: Block |
        _: Val | _: Ordering | _: QuotedReference | _: If
        ) =>
        fail(s"Invalid cypher: '$a'")
    }

  implicit def cypherQueryTokenizer(implicit strategy: NamingStrategy): Tokenizer[CypherQuery] = Tokenizer[CypherQuery] {
    case CypherQuery(node, label) =>
      stmt"MATCH (${node.token}:${node.token}) RETURN ${node.token}"
  }

  implicit def entityTokenizer(implicit strategy: NamingStrategy): Tokenizer[Entity] = Tokenizer[Entity] {
    case Entity(name, properties) => strategy.table(name).token
  }

  implicit def queryTokenizer(implicit strategy: NamingStrategy): Tokenizer[Query] = Tokenizer[Query] {
    case q => CypherQuery(q).token
  }

  implicit def operationTokenizer(implicit strategy: NamingStrategy): Tokenizer[Operation] = Tokenizer[Operation] {
    case BinaryOperation(a, op @ SetOperator.`contains`, b) => stmt"${b.token} ${op.token} (${a.token})"
    case BinaryOperation(a, op, b)                          => stmt"${a.token} ${op.token} ${b.token}"
    case e: UnaryOperation                                  => fail(s"Cql doesn't support unary operations. Found: '$e'")
    case e: FunctionApply                                   => fail(s"Cql doesn't support functions. Found: '$e'")
  }

  implicit val aggregationOperatorTokenizer: Tokenizer[AggregationOperator] = Tokenizer[AggregationOperator] {
    case AggregationOperator.`size` => stmt"COUNT"
    case o                          => fail(s"Cql doesn't support '$o' aggregations")
  }

  implicit val binaryOperatorTokenizer: Tokenizer[BinaryOperator] = Tokenizer[BinaryOperator] {
    case EqualityOperator.`==`  => stmt"="
    case BooleanOperator.`&&`   => stmt"AND"
    case NumericOperator.`>`    => stmt">"
    case NumericOperator.`>=`   => stmt">="
    case NumericOperator.`<`    => stmt"<"
    case NumericOperator.`<=`   => stmt"<="
    case NumericOperator.`+`    => stmt"+"
    case SetOperator.`contains` => stmt"IN"
    case other                  => fail(s"Cql doesn't support the '$other' operator.")
  }

  implicit def propertyTokenizer(implicit valueTokenizer: Tokenizer[Value], identTokenizer: Tokenizer[Ident], strategy: NamingStrategy): Tokenizer[Property] =
    Tokenizer[Property] {
      case Property(_, name) => strategy.column(name).token
    }

  implicit def valueTokenizer(implicit strategy: NamingStrategy): Tokenizer[Value] = Tokenizer[Value] {
    case Constant(v: String) => stmt"'${v.token}'"
    case Constant(())        => stmt"1"
    case Constant(v)         => stmt"${v.toString.token}"
    case Tuple(values)       => stmt"${values.token}"
    case NullValue           => fail("Cql doesn't support null values.")
  }

  implicit def infixTokenizer(implicit propertyTokenizer: Tokenizer[Property], strategy: NamingStrategy, queryTokenizer: Tokenizer[Query]): Tokenizer[Infix] = Tokenizer[Infix] {
    case Infix(parts, params) =>
      val pt = parts.map(_.token)
      val pr = params.map(_.token)
      Statement(Interleave(pt, pr))
  }

  implicit def identTokenizer(implicit strategy: NamingStrategy): Tokenizer[Ident] = Tokenizer[Ident] {
    case e => strategy.default(e.name).token
  }

  implicit def assignmentTokenizer(implicit propertyTokenizer: Tokenizer[Property], strategy: NamingStrategy): Tokenizer[Assignment] = Tokenizer[Assignment] {
    case Assignment(alias, prop, value) =>
      stmt"${prop.token} = ${value.token}"
  }

  implicit def actionTokenizer(implicit strategy: NamingStrategy): Tokenizer[Action] = {

    implicit def queryTokenizer(implicit strategy: NamingStrategy): Tokenizer[Query] = Tokenizer[Query] {
      case q: Entity => q.token
      case other     => fail(s"not implemented yet")
    }

    Tokenizer[Action] {
      case other =>
        fail(s"not implemented yet")
    }
  }

}
