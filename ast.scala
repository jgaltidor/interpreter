package mylang
import scala.util.parsing.input.Positional
import ExceptionUtils._

/* Expressions */
sealed trait Expr extends Positional {
  var etype: Type = null
}

trait Value extends Expr
case class Str(s : String) extends Value
case class Num(n : Int) extends Value
case class Var(name : String) extends Expr with Pattern
case class Plus(e1 : Expr, e2: Expr) extends Expr
case class Times(e1 : Expr, e2: Expr) extends Expr
case class Cat(e1 : Expr, e2: Expr) extends Expr
case class Len(e : Expr) extends Expr
case class Let(v : Var, varExp : Expr, body: Expr) extends Expr

// functions
case class Func(name: String, v : Var, inType : Type, outType:Type,
                funDef:Expr, body:Expr) extends Expr
case class Call(funcVar: Var, e: Expr) extends Expr

/** Class to represent a function value that can be applied
  * to an expression
  */
case class FuncVal(argVar: Var, funcDef: Expr) extends Value

// products
case object Triv extends Value with Pattern
case class Pair(e1 : Expr, e2 : Expr) extends Expr
case class PairVal(v1 : Value, v2 : Value) extends Pair(v1, v2) with Value

// projection
case class Projection(index : ProjIndex, e : Expr) extends Expr
sealed abstract class ProjIndex
case object LeftProj extends ProjIndex
case object RightProj extends ProjIndex

// sums
/** An expression that wraps another expression, innerExpr,
  * where innerExpr should be a type allowed by the
  * domainType.
  */
abstract class DomainExpr(var domainType : Type) extends Expr {
  def this() = this(null)
}
case class Abort(domType : Type, e : Expr)
  extends DomainExpr(domType)
case class AbortVal(override val domType : Type, v : Value)
  extends Abort(domType, v) with Value
case class InExpr(index : ProjIndex, e : Expr) extends DomainExpr
case class InVal(override val index : ProjIndex, v : Value)
  extends InExpr(index, v) with Value
case class Case(cond: Expr, v1 : Var, body1 : Expr,
                v2: Var, body2 : Expr) extends Expr
// patterns
trait Pattern extends Positional
case class Rule(vars: List[Var], pat: Pattern, exp : Expr) extends Positional
case class PairPattern(p1 : Pattern, p2 : Pattern)
  extends Pattern
case class InPattern(index : ProjIndex, p : Pattern)
  extends Pattern
case object WildPattern extends Pattern
case class MatchExpr(exp: Expr, rules: List[Rule]) extends Expr

// failures
trait ErrorVal extends Value {
  def errMsg():String
}
trait Failure extends ErrorVal
trait ExcVal extends ErrorVal

case class FailVal() extends Failure {
  override def errMsg():String = defaultErrorValMsg(this, "Unhandled failure")
}

case class MatchFailure(m: MatchExpr, mval: Value) extends Failure {
  override def errMsg():String = defaultErrorValMsg(m,
    "No rule matched value " + mval + " for match expression")
}

case class CatchExpr(tryExp: Expr, handleExp:Expr) extends Expr

case class RaiseExpr(e:Expr) extends Expr
case class HandleExpr(tryExp: Expr, v:Var, handleExp:Expr) extends Expr

// exception expressions
case class ExcMsg(e:Expr) extends Expr
case class ExcCode(e:Expr) extends Expr

// exception values
case class ExcMsgVal(override val v: Value) extends InVal(LeftProj, v) with ExcVal {
  override def errMsg():String = v match {
    case Str(s) => defaultErrorValMsg(this, "Message Exception: " + s)
  }
}
case class ExcCodeVal(override val v: Value) extends InVal(RightProj, v) with ExcVal {
  override def errMsg():String = v match {
    case Num(n) => defaultErrorValMsg(this, "Error Code Exception: " + n)
  }
}

// continuations
case class Letcc(v: Var, e: Expr) extends Expr
case class ThrowExpr(tryExp:Expr, contExp: Expr) extends Expr


/** Types */
abstract class Type extends Positional
case object NumType extends Type
case object StrType extends Type

// products
case class ProdType(t1 : Type, t2 : Type) extends Type
case object UnitType extends Type

// sums
case class SumType(var t1 : Type, var t2 : Type) extends Type
case object VoidType extends Type

// exception types
case object ExcType extends SumType(StrType, NumType)

// continuations
case class ContinuationType(val t: Type) extends Type


object ExprTester {

  def main(args: Array[String]) {
    println("Hello, world!")
    val s = "blah"
    s match {
      case "what" => println("dammit")
      case "blah" => println("yes!!!")
      case _ => println("OK")
    }
    val n = new Num(5) 
    println("n: " + n)
    val p = new PairVal(Num(7), Str("y"))
    print("p: " + p)
    if(p.isInstanceOf[Pair]) { println("p is a pair") }
    if(p.isInstanceOf[Value]) { println("p is a value") }
  }
}
