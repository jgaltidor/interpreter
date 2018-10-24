package mylang
import Utils._
import scala.util.parsing.input.Positional
import java.lang.String.format

class CompilerException(val msg: String) extends Exception(msg)

class PositionalException(positional: Positional, description:String) extends CompilerException(
  ExceptionUtils.defaultPositionalMsg(positional, description)
)

class NotInScope(variable: Var) extends PositionalException(variable,
  "Not found: variable " + variable.name
)

class MismatchError[A,B](positional: Positional, found: A, required: B, description: String) extends
  PositionalException(positional, description + "\n" +
    ExceptionUtils.tab + "found: " + found + "\n" +
    ExceptionUtils.tab + "required: " + required
  )
{
  def this(positional: Positional, found: A, required: B) = this(positional, found, required, "")
}

class TypeMismatch(expr: Expr, found: Type, required: Type, description: String)
  extends MismatchError[Type, Type](expr, found, required, "type mismatch; " + description)
{
  def this(expr: Expr, found: Type, required: Type) = this(expr, found, required, "")
}

class PatternTypeMismatch(pat: Pattern, patType: Type, condType: Type) extends PositionalException(pat,
  "Pattern type cannot match values of match condition expression type\n" +
    ExceptionUtils.tab + "Condition expression type: " + condType + "\n" +
    ExceptionUtils.tab + "Pattern type: " + patType
  )

class PatternCannotMatch(pat: Pattern, condType: Type) extends PositionalException(pat,
  "Pattern cannot type to match condition expression type: " + condType
)


class NoMatchError(m: MatchExpr, mval: Value) extends CompilerException(
  m.pos.line.toString + ": " + colorstr("Runtime error: ", RED) + 
  "No rule matched value " + mval + " for match expression\n" + m.pos.longString
)

object ExceptionUtils {

  def defaultPositionalMsg(positional: Positional, description: String): String =
    positional.pos.line.toString + ": " + colorstr("Error: ", RED) + 
    description + "\n" + positional.pos.longString

  def defaultErrorValMsg(positional: Positional, description: String): String =
    positional.pos.line.toString + ":\n" + description + "\n" +
      positional.pos.longString

  val tab = "  "
}
