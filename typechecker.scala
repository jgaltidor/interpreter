package mylang
import Utils._
import ExceptionUtils._
import java.lang.String.format

object TypeChecker
{
  class TypeException(msg: String) extends CompilerException(msg)
  class BadFuncDefinition(msg: String) extends CompilerException(msg)
  def typeError(msg: String) = throw new TypeException(msg)

  /** Representing function types */
  private case class ArrType(inType: Type, outType: Type) extends Type

  /** Type that signals any other type could be used instead */
  private case object AnyType extends Type

  /** Represents a type that is a subtype of every type */
  // private case class RuleType(patType : type, expType: Type) extends Type

  /** A mapping from variable name to types */
  private type Environment = ScopedEnvironment[Type]
  private val emptyEnv: Environment = new Environment

  sealed abstract class TypChkResult
  case class Success(etype: Type) extends TypChkResult
  case class Failure(exc: CompilerException) extends TypChkResult

  def typchk(expr: Expr): TypChkResult = {
    try {
      typchk(expr, emptyEnv) match {
        case t : ArrType => typeError(
          "Type of program expression should not be a function type")
        case t => Success(t)
      }
    }
    catch { case e:CompilerException => Failure(e) }
  }

  def typchk(expr: Expr, env: Environment):Type = typchk(expr, env, AnyType)

  def typchk(expr: Expr, env: Environment, expectedType: Type): Type = {
    expr.etype = expr match {
      case Str(s) => StrType
      case Num(n) => NumType
      case v:Var =>
        val typ = env(v)
        if(typ == AnyType) expectedType else typ
      case Plus(e1, e2) =>
        (typchk(e1, env, NumType), typchk(e2, env, NumType)) match {
          case (NumType, NumType) => NumType
          case (t, NumType) => throw new TypeMismatch(e1, t, NumType)
          case (_, t) => throw new TypeMismatch(e2, t, NumType)
      }
      case Times(e1, e2) =>
        (typchk(e1, env, NumType), typchk(e2, env, NumType)) match {
          case (NumType, NumType) => NumType
          case (t, NumType) => throw new TypeMismatch(e1, t, NumType)
          case (_, t) => throw new TypeMismatch(e2, t, NumType)
      }
      case Cat(e1, e2) =>
        (typchk(e1, env, StrType), typchk(e2, env, StrType)) match {
          case (StrType, StrType) => StrType
          case (t, StrType) => throw new TypeMismatch(e1, t, StrType)
          case (_, t) => throw new TypeMismatch(e2, t, StrType)
      }
      case Len(e) => typchk(e, env, StrType) match {
        case StrType => NumType
        case t => throw new TypeMismatch(e, t, StrType)
      }
      case Let(v, varExp, body) =>
        // assign to v.etype since v does not necessarily appear in the body
        // then v may never be type checked and assigned a type
        v.etype = typchk(varExp, env)
        val extendedEnv = new Environment(env)
        extendedEnv += v -> v.etype
        typchk(body, extendedEnv, expectedType)
      // function cases
      case Func(name, v, inType, outType, funDef, body) =>
        if(name == v.name) {
          throw new PositionalException(expr,
            "Function name and argument have the same name: " + name)
        }
        v.etype = inType
        val extendedEnv1 = new Environment(env)
        extendedEnv1 += v -> v.etype
        extendedEnv1 += Var(name) -> ArrType(inType, outType)
        val funDefType = typchk(funDef, extendedEnv1, outType)
        if(subTypeOf(funDefType, outType)) {
          val extendedEnv2 = new Environment(env)
          extendedEnv2 += Var(name) -> ArrType(inType, outType)
          typchk(body, extendedEnv2, expectedType)
        }
        else {
          throw new TypeMismatch(funDef, funDefType, outType, format(
            "Function definition type of %s is not of its return type", name))
        }
      case Call(fvar, exp) =>
        val (argType, outType) = typchk(fvar, env) match {
          case ArrType(iType, oType) => (iType, oType)
          case _ => throw new PositionalException(expr,
            "Attempting to call a non-function expression")
        }
        val inType = typchk(exp, env, argType)
        if(subTypeOf(inType, argType)) {
          outType
        }
        else {
          throw new TypeMismatch(exp, inType, argType,
          "Input expr type does not have arg type for function " + fvar.name)
        }
      // product cases
      case Triv => UnitType
      case Pair(e1, e2) => expectedType match {
        case ProdType(t1, t2) => ProdType(typchk(e1, env, t1), typchk(e2, env, t2))
        case _ => ProdType(typchk(e1, env), typchk(e2, env))
      }
      case Projection(index, e) =>
        val newExpectedType = index match {
          case LeftProj => ProdType(expectedType, AnyType)
          case RightProj => ProdType(AnyType, expectedType)
        }
        typchk(e, env, newExpectedType) match {
          case ProdType(t1, t2) => index match {
            case LeftProj => t1
            case RightProj => t2
          }
          case t => throw new MismatchError(e, t, "ProdType")
        }
      // sum cases
      case Abort(domainType, e) => typchk(e, env, VoidType) match {
        case VoidType => domainType
        case t => throw new TypeMismatch(e, t, VoidType)
      }
      case InExpr(LeftProj, e) =>
        val inexp = expr.asInstanceOf[InExpr]
        inexp.domainType = expectedType match {
          case SumType(t1, t2) => SumType(typchk(e, env, t1), t2)
          case _ => SumType(typchk(e, env), AnyType)
        }
        inexp.domainType
      case InExpr(RightProj, e) =>
        val inexp = expr.asInstanceOf[InExpr]
        inexp.domainType = expectedType match {
          case SumType(t1, t2) => SumType(t1, typchk(e, env, t2))
          case _ => SumType(AnyType, typchk(e, env))
        }
        inexp.domainType
      case Case(cond, v1, body1, v2, body2) => typchk(cond, env) match {
        case SumType(t1, t2) =>
          v1.etype = t1
          v2.etype = t2
          val extendedEnv1 = new Environment(env)
          extendedEnv1 += v1 -> v1.etype
          val bodyType1 = typchk(body1, extendedEnv1, expectedType)
          val extendedEnv2 = new Environment(env)
          extendedEnv2 += v2 -> v2.etype
          val bodyType2 = typchk(body2, extendedEnv2, expectedType)
          if(!areSameType(bodyType1, bodyType2)) throw new PositionalException(expr,
            "types of branches in case expression do not match\n" +
              tab + "branch1 type: " + bodyType1 + "\n" +
              tab + "branch2 type: " + bodyType2
            )
          bodyType1
        case t => throw new MismatchError(cond, t, "SumType")
      }
      // Type of match expression is type for all rule body expressions
      case MatchExpr(cond, rules) =>
        val inType = typchk(cond, env)
        var previousRuleType:Type = null
        var previousRule:Rule = null
        var i = 1
        for(rule <- rules) {
          val ruleType = typchk(rule, env, inType, expectedType)
          if(previousRuleType != null && !areSameType(ruleType, previousRuleType))
            throw new PositionalException(expr,
              "bodies of two rules in match expression do not match\n" +
                tab + "rule" + (i-1) + "  type: " + previousRuleType + "\n" +
                tab + "rule" + i + "  type: " + ruleType
            )
          previousRuleType = ruleType
          i = i + 1
        }
        previousRuleType
      case FailVal() => expectedType
      case CatchExpr(tryExp, handleExp) =>
        val tryType = typchk(tryExp, env, expectedType)
        val expectedHandType = expectedType match {
          case AnyType => tryType
          case t => t
        }
        val handleType = typchk(handleExp, env, expectedHandType)
        if(!areSameType(tryType, handleType)) throw new PositionalException(expr,
          "types of branches in catch expression do not match\n" +
            tab + "try type: " + tryType + "\n" +
            tab + "handle type: " + handleType
          )
        tryType
      case RaiseExpr(e) => typchk(e, env, ExcType) match {
        case ExcType => expectedType
        case t => throw new TypeMismatch(e, t, ExcType)
      }
      case HandleExpr(tryExp, v, handleExp) =>
        val tryType = typchk(tryExp, env, expectedType)
        val expectedHandType = expectedType match {
          case AnyType => tryType
          case t => t
        }
        val extendedEnv = new Environment(env)
        extendedEnv += v -> ExcType
        val handleType = typchk(handleExp, extendedEnv, expectedHandType)
        if(!areSameType(tryType, handleType)) throw new PositionalException(expr,
          "types of branches in handler expression do not match\n" +
            tab + "try type: " + tryType + "\n" +
            tab + "handle type: " + handleType
          )
        tryType
      case ExcMsg(e) => typchk(e, env, StrType) match {
        case StrType => ExcType
        case t => throw new TypeMismatch(e, t, StrType)
      }
      case ExcCode(e) => typchk(e, env, NumType) match {
        case NumType => ExcType
        case t => throw new TypeMismatch(e, t, NumType)
      }
      case Letcc(v, e) =>
        val expectedContType = ContinuationType(expectedType)
        v.etype = expectedContType
        val extendedEnv = new Environment(env)
        extendedEnv += v -> v.etype
        typchk(e, extendedEnv, expectedContType.t)
      case ThrowExpr(tryExp, contExp) =>
        val tryType = typchk(tryExp, env)
        val expectedContType = ContinuationType(tryType)
        val contType = typchk(contExp, env, expectedContType)
        contType match {
          case ContinuationType(t) =>
            if (!subTypeOf(expectedContType.t, t)) {
              throw new PositionalException(expr,
               "Continuation of throw expression does not accept type of try branch\n" +
                  tab + "try type: " + tryType + "\n" +
                  tab + "continuation type: " + contType
              )
            }
            expectedType
          case t => throw new TypeMismatch(contExp, t, expectedContType)
        }
      case v:FuncVal => throw new RuntimeException("Should not reach here")
      case v:MatchFailure => throw new RuntimeException("Should not reach here") 
      case v:ExcVal => throw new RuntimeException("Should not reach here")
    }
    expr.etype
  }

  def typchk(rule: Rule, env: Environment, inType: Type, expectedType: Type):Type = {
    getDuplicate(rule.vars) match {
      case Some(v) => throw new PositionalException(rule, format(
      "Variable %s appears twice in rule", v))
      case _ =>
    }
    for(v <- rule.vars) {
      // check that no variables in the rule is contained in the environment
      if(env.containsInScope(v)) throw new PositionalException(rule,
        format("Rule variable %s defined in outer scope", v)
      )
    }
    val (patEnv, patType) = typchk(rule.pat, inType)
    if(!subTypeOf(inType, patType))
      throw new PatternTypeMismatch(rule.pat, patType, inType)
    patEnv.outerEnv = env
    typchk(rule.exp, patEnv, expectedType)
  }
  
  def typchk(pat: Pattern, inType: Type):(Environment, Type) = (pat, inType) match {
      case (v:Var, t) =>
        val env = new Environment
        env += v -> t
        (env, t)
      // case (InPattern(LeftProj, p), SumType(VoidType, t2)) => throw new PatternCannotMatch(pat, inType)
      case (InPattern(LeftProj, p), SumType(t1, t2)) =>
        val (env, t) = typchk(p, t1)
        (env, SumType(t, t2))
      // case (InPattern(RightProj, p), SumType(t1, VoidType)) => throw new PatternCannotMatch(pat, inType)
      case (InPattern(RightProj, p), SumType(t1, t2)) =>
        val (env, t) = typchk(p, t2)
        (env, SumType(t1, t))
      case (PairPattern(p1, p2), ProdType(t1, t2)) =>
        val (env1, typ1) = typchk(p1, t1)
        val (env2, typ2) = typchk(p2, t2)
        getSharedElement(env1.keys, env2.keys) match {
          case Some(v) => throw new PositionalException(pat, format(
           "Variable %s appears on both left and right hand side pair pattern: %s", v))
          case None =>
            env1 ++= env2
            (env1, ProdType(typ1, typ2))
        }
      case (Triv, _) => (new Environment, UnitType)
      case (WildPattern, _) => (new Environment, inType)
      case (p, t) => throw new PatternCannotMatch(p, t)
    }

  def subTypeOf(type1: Type, type2: Type):Boolean =
    if(type1 == type2) return true
    else (type1, type2) match {
      case (AnyType, _) => true
      case (_, AnyType) => true
      case (VoidType, _) => true
      case (SumType(x1, y1), SumType(x2, y2)) =>
        subTypeOf(x1, x2) && subTypeOf(y1, y2)
      case (ProdType(x1, y1), ProdType(x2, y2)) =>
        subTypeOf(x1, x2) && subTypeOf(y1, y2)
      case _ => false
  }

  def areSameType(type1: Type, type2: Type):Boolean =
    if(type1 == type2) return true
    else return (type1 == AnyType || type2 == AnyType)

  def hasType(expr: Expr, env: Environment, typ: Type):(Boolean, Type) =
    (expr, typ) match {
      case (InExpr(LeftProj, e), SumType(t1, t2)) =>
        hasType(e, env, t1)
      case (InExpr(RightProj, e), SumType(t1, t2)) =>
        hasType(e, env, t2)
      case _ =>
        val etype = typchk(expr, env)
        (subTypeOf(etype, typ), etype)
    }

  def printtype(expr : Expr) = print(typstr(expr))

  def typstr(expr: Expr):String = {
    val s = expr match {
      case v:Value => v.toString
      case v:Var => v.toString
      case Plus(e1, e2) =>
        format("Plus(%s,%s)", typstr(e1), typstr(e2))
      case Times(e1, e2) =>
        format("Times(%s,%s)", typstr(e1), typstr(e2))
      case Cat(e1, e2) =>
        format("Cat(%s,%s)", typstr(e1), typstr(e2))
      case Len(e) =>
        format("Len(%s)", typstr(e))
      case Let(v, e1, e2) =>
        format("Let(%s,%s,%s)", typstr(v), typstr(e1), typstr(e2))
      case Func(name, v, inType, outType, funDef, body) =>
        format("Func(%s,%s:%s):%s=%n%s in%n %s",
          name, typstr(v), inType, outType, typstr(funDef), typstr(body))
      case Call(fvar, e) =>
        format("Call(%s,%s)", typstr(fvar), typstr(e))
      case Pair(e1, e2) =>
        format("Pair(%s,%s)", typstr(e1), typstr(e2))
      case Projection(index, e) =>
        format("Projection(%s,%s)", index, typstr(e))
      case Abort(domainType, e) =>
        format("Abort(%s,%s)", domainType, typstr(e))
      case InExpr(index, e) =>
        format("InExpr(%s,%s)", index, typstr(e))
      case Case(cond, v1, body1, v2, body2) =>
        format("Case(%s,%s,%s,%s,%s)",
          typstr(cond), typstr(v1), typstr(body1), typstr(v2), typstr(body2))
      case MatchExpr(e, rules) =>
        format("MatchExpr(%s,%n", typstr(e)) +
          rules.foldLeft("")((s, r) => s + "\t" + typstr(r) + "\n") +
          ")"
      case CatchExpr(e1, e2) => 
        format("CatchExpr(%s,%s)", typstr(e1), typstr(e2))
      case RaiseExpr(e) => format("RaiseExpr(%s)", typstr(e))
      case HandleExpr(e1, v, e2) =>
        format("HandleExpr(%s, %s, %s)", typstr(e1), typstr(v), typstr(e2))
      case ExcCode(e) => format("ExcCode(%s)", typstr(e))
      case ExcMsg(e) => format("ExcMsg(%s)", typstr(e))
      case Letcc(v, e) => 
        format("Letcc(%s,%s)", typstr(v), typstr(e))
      case ThrowExpr(tryExp, contExp) => 
        format("ThrowExpr(%s,%s)", typstr(tryExp), typstr(contExp))
    }
    s + colorstr(":" + expr.etype)
  }

  def typstr(r : Rule): String = format("Rule(%s,%s,%s)", r.vars, r.pat, typstr(r.exp))
}
