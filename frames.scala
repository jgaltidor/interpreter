package mylang

// Defining frames that can be pushed on the
// control stack

sealed abstract class Frame

case class PlusRightFrame(e2:Expr) extends Frame
case class PlusLeftFrame(v1:Value) extends Frame

case class TimesRightFrame(e2:Expr) extends Frame
case class TimesLeftFrame(v1:Value) extends Frame

case class CatRightFrame(e2:Expr) extends Frame
case class CatLeftFrame(v1:Value) extends Frame

case class PairRightFrame(e2:Expr) extends Frame
case class PairLeftFrame(v1:Value) extends Frame

case class LetFrame(v: Var, body:Expr) extends Frame

case object LenFrame extends Frame

case class CallFrame(f:FuncVal) extends Frame

case class ProjFrame(index: ProjIndex) extends Frame

case class InjFrame(index: ProjIndex) extends Frame

case class CaseFrame(v1 : Var, body1 : Expr,
                v2: Var, body2 : Expr) extends Frame

case class MatchFrame(matchexp:MatchExpr) extends Frame

case class AbortFrame(typ: Type) extends Frame

case class CatchFrame(handleExp: Expr) extends Frame

case object RaiseFrame extends Frame

case class ErrMsgFrame(exc:Expr) extends Frame

case class ErrCodeFrame(exc:Expr) extends Frame

case class HandleFrame(v: Var, handleExp: Expr) extends Frame

case object DoNothingFrame extends Frame

case class ThrowRightFrame(contExp:Expr) extends Frame
case class ThrowLeftFrame(tryVal:Value) extends Frame
