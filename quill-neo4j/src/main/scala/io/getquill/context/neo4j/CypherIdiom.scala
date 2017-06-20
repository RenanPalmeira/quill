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

  override def liftingPlaceholder(idx: Int) = ""

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
    case CypherQuery(node, filter, select) =>

      val withEntity = stmt"MATCH ${node.token}"

      val withFilter =
        filter match {
          case None    => withEntity
          case Some(f) => stmt"$withEntity WHERE ${f.token}"
        }

      stmt"$withFilter RETURN ${select.token}"
  }

  implicit def entityTokenizer(implicit strategy: NamingStrategy): Tokenizer[Entity] = Tokenizer[Entity] {
    case Entity(name, properties) => strategy.table(name).token
  }

  implicit def queryTokenizer(implicit strategy: NamingStrategy): Tokenizer[Query] = Tokenizer[Query] {
    case q => CypherQuery(q).token
  }

  implicit def operationTokenizer(implicit strategy: NamingStrategy): Tokenizer[Operation] = Tokenizer[Operation] {
    case UnaryOperation(op, ast)                              => stmt"${op.token} (${ast.token})"
    case BinaryOperation(a, EqualityOperator.`==`, NullValue) => stmt"${scopedTokenizer(a)} IS NULL"
    case BinaryOperation(NullValue, EqualityOperator.`==`, b) => stmt"${scopedTokenizer(b)} IS NULL"
    case BinaryOperation(a, EqualityOperator.`!=`, NullValue) => stmt"${scopedTokenizer(a)} IS NOT NULL"
    case BinaryOperation(NullValue, EqualityOperator.`!=`, b) => stmt"${scopedTokenizer(b)} IS NOT NULL"
    case BinaryOperation(a, op @ SetOperator.`contains`, b)   => stmt"${scopedTokenizer(b)} ${op.token} (${a.token})"
    case BinaryOperation(a, op, b)                            => stmt"${scopedTokenizer(a)} ${op.token} ${scopedTokenizer(b)}"
    case e: FunctionApply                                     => fail(s"Can't translate the ast to Cypher: '$e'")
  }

  implicit val unaryOperatorTokenizer: Tokenizer[UnaryOperator] = Tokenizer[UnaryOperator] {
    case NumericOperator.`-`          => stmt"-"
    case BooleanOperator.`!`          => stmt"NOT"
    case StringOperator.`toUpperCase` => stmt""
    case StringOperator.`toLowerCase` => stmt""
    case StringOperator.`toLong`      => stmt"" // cast is implicit
    case StringOperator.`toInt`       => stmt"" // cast is implicit
    case SetOperator.`isEmpty`        => stmt""
    case SetOperator.`nonEmpty`       => stmt""
  }

  implicit val aggregationOperatorTokenizer: Tokenizer[AggregationOperator] = Tokenizer[AggregationOperator] {
    case o => fail(s"Cypher doesn't support '$o' aggregations")
  }

  implicit val binaryOperatorTokenizer: Tokenizer[BinaryOperator] = Tokenizer[BinaryOperator] {
    case EqualityOperator.`==` => stmt"="
    case BooleanOperator.`&&`  => stmt"AND"
    case BooleanOperator.`||`  => stmt"OR"
    case NumericOperator.`>`   => stmt">"
    case NumericOperator.`>=`  => stmt">="
    case NumericOperator.`<`   => stmt"<"
    case NumericOperator.`<=`  => stmt"<="
    case other                 => fail(s"Cypher doesn't support the '$other' operator.")
  }

  implicit def propertyTokenizer(implicit valueTokenizer: Tokenizer[Value], identTokenizer: Tokenizer[Ident], strategy: NamingStrategy): Tokenizer[Property] = {
    def unnest(ast: Ast): Ast =
      ast match {
        case Property(a, _) => unnest(a)
        case a              => a
      }

    Tokenizer[Property] {
      case Property(ast, name) => stmt"${scopedTokenizer(unnest(ast))}.${strategy.column(name).token}"
    }
  }

  implicit def valueTokenizer(implicit strategy: NamingStrategy): Tokenizer[Value] = Tokenizer[Value] {
    case Constant(v: String) => stmt"'${v.token}'"
    case Constant(())        => stmt"1"
    case Constant(v)         => stmt"${v.toString.token}"
    case Tuple(values)       => stmt"${values.token}"
    case NullValue           => fail("Cypher doesn't support null values.")
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

  implicit def sourceTokenizer(implicit strategy: NamingStrategy): Tokenizer[FromContext] = Tokenizer[FromContext] {
    case NodeContext(name, alias) => stmt"(${strategy.default(alias).token}:${name.token})"
  }

  protected def scopedTokenizer[A <: Ast](ast: A)(implicit token: Tokenizer[A]) =
    ast match {
      case _: Query           => stmt"(${ast.token})"
      case _: BinaryOperation => stmt"(${ast.token})"
      case _: Tuple           => stmt"(${ast.token})"
      case _                  => ast.token
    }

}
