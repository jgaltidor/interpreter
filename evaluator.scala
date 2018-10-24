package mylang
import Utils._
import java.lang.String.format
import scala.collection.immutable.Stack

object Evaluator
{
	/** A mapping from variable name to expressions they're bound to */
	/** A mapping from variable name to types */
	private type Environment = ScopedEnvironment[Value]
	private val emptyEnv: Environment = new Environment
	private type FrameState = (Frame, Environment)
	private val emptyStack = new Stack[FrameState]


	case class ContinuationVal(ctrstk: Stack[FrameState]) extends Value

	def eval(e:Expr): Value = evalExpr(e, emptyStack) match {
		case v: FuncVal =>
		  throw new RuntimeException("Expression should not evaluate to a function")
		case v => v
	}

  /** New evaluation function */
	def evalExpr(expr:Expr, ctrstk: Stack[FrameState]): Value = {
	  // println(format("evalExpr(%s > %s)", ctrstk, expr))
	  // Getting the current environment
	  val env = if (ctrstk.isEmpty) emptyEnv else ctrstk.top._2
	  expr match {
			case f:FailVal =>
				evalFailReturn(f, ctrstk)
			case v:Value =>
				evalReturn(v, ctrstk)
		  case v:Var => evalReturn(env(v), ctrstk)
			case Plus(e1, e2) =>
				evalExpr(e1, ctrstk.push((PlusRightFrame(e2), env)))
			case Times(e1, e2) =>
				evalExpr(e1, ctrstk.push((TimesRightFrame(e2), env)))
			case Cat(e1, e2) =>
				evalExpr(e1, ctrstk.push((CatRightFrame(e2), env)))
			case Pair(e1, e2) =>
				evalExpr(e1, ctrstk.push((PairRightFrame(e2), env)))
			case Let(v, varExp, body) =>
				evalExpr(varExp, ctrstk.push((LetFrame(v, body), env)))
			case Len(e) => 
				evalExpr(e, ctrstk.push((LenFrame, env)))
			case Func(name, argVar, inType, outType, funDef, body) =>
				val extendedEnv = new Environment(env)
				extendedEnv += Var(name) -> FuncVal(argVar, funDef)
				evalExpr(body, ctrstk.push((DoNothingFrame, extendedEnv)))
			case Call(funcVar, exp) =>
				val f = env(funcVar) match { case f:FuncVal => f }
				evalExpr(exp, ctrstk.push((CallFrame(f), env)))
			case Projection(index, e) =>
				evalExpr(e, ctrstk.push((ProjFrame(index), env)))
			case Abort(typ, e) =>
				evalExpr(e, ctrstk.push((AbortFrame(typ), env)))
			case InExpr(index, e) =>
				evalExpr(e, ctrstk.push((InjFrame(index), env)))
			case Case(cond, var1, body1, var2, body2) =>
				evalExpr(cond, ctrstk.push((CaseFrame(var1, body1, var2, body2), env)))
			case m:MatchExpr =>
				evalExpr(m.exp, ctrstk.push((MatchFrame(m), env)))
			case CatchExpr(tryExp, handleExp) =>
				evalExpr(tryExp, ctrstk.push((CatchFrame(handleExp), env)))
			case RaiseExpr(e) =>
				evalExpr(e, ctrstk.push((RaiseFrame, env)))
			case ExcMsg(e) =>
				evalExpr(e, ctrstk.push((ErrMsgFrame(expr), env)))
			case ExcCode(e) =>
				evalExpr(e, ctrstk.push((ErrCodeFrame(expr), env)))
			case HandleExpr(tryExp, v, handleExp) =>
				evalExpr(tryExp, ctrstk.push((HandleFrame(v, handleExp), env)))
			case Letcc(v, e) =>
				val extendedEnv = new Environment(env)
				extendedEnv += v -> ContinuationVal(ctrstk)
				evalExpr(e, ctrstk.push((DoNothingFrame, extendedEnv)))
			case ThrowExpr(tryExp, contExp) =>
				evalExpr(tryExp, ctrstk.push((ThrowRightFrame(contExp), env)))
		}
	}

	def evalReturn(value:Value, ctrstk: Stack[FrameState]): Value = {
		// println(format("evalReturn(%s < %s)", ctrstk, value))
		if(ctrstk isEmpty) {
			value
		}
		else {
	  	// Getting the current environment
	  	val env = ctrstk.top._2
			ctrstk.top._1 match {
				case PlusRightFrame(e2) =>
					evalExpr(e2, ctrstk.pop.push((PlusLeftFrame(value), env)))
				case TimesRightFrame(e2) =>
					evalExpr(e2, ctrstk.pop.push((TimesLeftFrame(value), env)))
				case CatRightFrame(e2) =>
					evalExpr(e2, ctrstk.pop.push((CatLeftFrame(value), env)))
				case PairRightFrame(e2) =>
					evalExpr(e2, ctrstk.pop.push((PairLeftFrame(value), env)))
				case PlusLeftFrame(v1) => (v1, value) match {
					case (Num(n1), Num(n2)) =>
						evalReturn(Num(n1+n2), ctrstk.pop)
				}
				case TimesLeftFrame(v1) => (v1, value) match {
					case (Num(n1), Num(n2)) =>
						evalReturn(Num(n1*n2), ctrstk.pop)
				}
				case CatLeftFrame(v1) => (v1, value) match {
					case (Str(s1), Str(s2)) =>
						evalReturn(Str(s1+s2), ctrstk.pop)
				}
				case PairLeftFrame(v1) =>
					evalReturn(PairVal(v1, value), ctrstk.pop)
				case LetFrame(v, body) =>
					val extendedEnv = new Environment(env)
					extendedEnv += v -> value
					evalExpr(body, ctrstk.pop.push((DoNothingFrame, extendedEnv)))
				case LenFrame => value match {
					case Str(s) => evalReturn(Num(s.length), ctrstk.pop)
				}
				case CallFrame(FuncVal(argVar, funcDef)) =>
					val extendedEnv = new Environment(env)
					extendedEnv += argVar -> value
					evalExpr(funcDef, ctrstk.pop.push((DoNothingFrame, extendedEnv)))
				case ProjFrame(LeftProj) => value match {
					case PairVal(v1, v2) => evalReturn(v1, ctrstk.pop)
				}
				case ProjFrame(RightProj) => value match {
					case PairVal(v1, v2) => evalReturn(v2, ctrstk.pop)
				}
				case InjFrame(index) => evalReturn(InVal(index, value), ctrstk.pop)
				case AbortFrame(typ) => evalReturn(AbortVal(typ, value), ctrstk.pop)
				case CaseFrame(var1, body1, var2, body2) => value match {
					case InVal(LeftProj, v) =>
						val extendedEnv = new Environment(env)
						extendedEnv += var1 -> v
						evalExpr(body1, ctrstk.pop.push((DoNothingFrame, extendedEnv)))
					case InVal(RightProj, v) =>
						val extendedEnv = new Environment(env)
						extendedEnv += var2 -> v
						evalExpr(body2, ctrstk.pop.push((DoNothingFrame, extendedEnv)))
				}
				case m:MatchFrame =>
					getRuleEnv(value, m.matchexp.rules) match {
						case Some((rule, patEnv)) =>
							patEnv.outerEnv = env
							evalExpr(rule.exp, ctrstk.pop.push((DoNothingFrame, patEnv)))
						case None =>
							evalExpr(MatchFailure(m.matchexp, value), ctrstk.pop)
					}
				case CatchFrame(handleExp) =>
					evalReturn(value, ctrstk.pop)
				case RaiseFrame =>
					evalFailReturn(value, ctrstk.pop)
				case ErrMsgFrame(exc) =>
					val v = ExcMsgVal(value)
					v.setPos(exc.pos)
					evalReturn(v, ctrstk.pop)
				case ErrCodeFrame(exc) =>
					val v = ExcCodeVal(value)
					v.setPos(exc.pos)
					evalReturn(v, ctrstk.pop)
				case HandleFrame(v, handleExp) =>
					evalReturn(value, ctrstk.pop)
				case DoNothingFrame =>
					evalReturn(value, ctrstk.pop)
				case ThrowRightFrame(contExp) =>
					evalExpr(contExp, ctrstk.pop.push((ThrowLeftFrame(value), env)))
				case ThrowLeftFrame(v) => value match {
					case ContinuationVal(stk) => evalReturn(v, stk)
				}
			}
		}
	}

	def evalFailReturn(err:Value, ctrstk: Stack[FrameState]): Value = {
		// println(format("evalFailReturn(%s)", ctrstk))
		if(ctrstk.isEmpty) {
			err
		}
		else {
	  	// Getting the current environment
	  	val env = ctrstk.top._2
			(ctrstk.top._1, err) match {
				case (CatchFrame(handleExp), f:Failure) =>
					evalExpr(handleExp, ctrstk.pop)
				case (HandleFrame(v, handleExp), e:ExcVal) =>
					val extendedEnv = new Environment(env)
					extendedEnv += v -> err
					evalExpr(handleExp, ctrstk.pop.push((DoNothingFrame, extendedEnv)))
				case _ => evalFailReturn(err, ctrstk.pop)
			}
		}
	}

  private def getRuleEnv(mval:Value, rules:List[Rule]):Option[(Rule, Environment)] =
		rules match {
			case Nil => None
			case r::rs => getRuleEnv(mval, r) match {
				case None => getRuleEnv(mval, rs)
				case v => v
			}
		}

  private def getRuleEnv(mval:Value, rule:Rule):Option[(Rule, Environment)] =
		getEnvironment(rule.pat, mval) match {
			case Some(patEnv) => Some(rule, patEnv)
			case None => None
		}

	private def getEnvironment(pat: Pattern, value: Value):Option[Environment] =
		(pat, value) match {
			case (v:Var, _) =>
				val env = new Environment
				env += (v -> value)
				Some(env)
			case (Triv, Triv) => Some(new Environment)
			case (WildPattern, _) => Some(new Environment)
			case (InPattern(LeftProj, p), InVal(LeftProj, v)) => getEnvironment(p, v)
			case (InPattern(RightProj, p), InVal(RightProj, v)) => getEnvironment(p, v)
			case (PairPattern(p1, p2), PairVal(v1, v2)) =>
				(getEnvironment(p1, v1), getEnvironment(p2, v2)) match {
					case (Some(env1), Some(env2)) =>
						env1 ++= env2
						Some(env1)
					case _ => None
				}
			case _ => None
	}

}
