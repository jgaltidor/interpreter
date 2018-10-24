package mylang
import scala.util.parsing.combinator.syntactical._

object MyParser extends StandardTokenParsers
{  
  lexical.delimiters ++= List("+", "*", "|", "^", ":", "=", "(", ")",
                              "<", ">", ",", "{", "}", "=>", ".", "_",
                              "[", "]")

  lexical.reserved ++= List("let", "be", "in", "fun", "str", "num",
                            "prl", "prr", "unit", "case",
                            "abort", "void", "match", "fail", "try", "ow",
                            "errmsg", "errcode", "raise", "cont", "letcc",
                            "throw", "to")

  def expr: Parser[Expr] = positioned(
      term ~ "+" ~ expr ^^ { case left ~ "+" ~ right => Plus(left, right) }
    | term ~ "^" ~ expr ^^ { case left ~ "^" ~ right => Cat(left, right) }
    | term
    ||| "let" ~> ident ~ ("be" ~>  expr <~ "in") ~ expr ^^
          { case id ~ left ~ right => Let(Var(id), left, right) }
    ||| funcDef
    ||| sumExpr
    ||| matchExpr
    ||| "try" ~> expr ~ ("ow" ~> ident <~ "=>") ~ expr ^^
          { case left ~ id ~ right => HandleExpr(left, Var(id), right) }
    ||| "try" ~> expr ~ "ow" ~ expr ^^
          { case left ~ "ow" ~ right => CatchExpr(left, right) }
    ||| "letcc" ~> ident ~ ("in" ~> expr) ^^
          { case id ~ exp => Letcc(Var(id), exp) }
    ||| "throw" ~> expr ~ ("to" ~> expr) ^^
          { case tryExp ~ contExp => ThrowExpr(tryExp, contExp) }
    )

  def term: Parser[Expr] = positioned(
      factor ~ "*" ~ expr   ^^ { case left ~ "*" ~ right => Times(left, right) }
    | factor
    )

  def factor: Parser[Expr] = positioned(
      proj
    | pair
    | funcCall
    | ident                          ^^ { Var(_) }
    |  numericLit                     ^^ { case numStr => Num(numStr.toInt) }
    | stringLit                      ^^ { Str(_) }
    | "|" ~> expr <~ "|"             ^^ { Len(_) }
    | "fail"                         ^^^ { FailVal() }
    | "errmsg" ~ "(" ~> expr <~ ")"  ^^ { ExcMsg(_) }
    | "errcode" ~ "(" ~> expr <~ ")" ^^ { ExcCode(_) }
    | "raise" ~ "(" ~> expr <~ ")"   ^^ { RaiseExpr(_) }
    )

  def funcDef: Parser[Expr] = positioned(
    ("fun" ~> ident) ~
      ("(" ~> ident <~ ":") ~
      (typeName <~ ")" ~ ":") ~
      (typeName <~ "=") ~
      (expr <~ "in") ~ expr ^^
        { case funcName ~ arg ~ argType ~ outType ~ fDef ~ body =>
             Func(funcName, Var(arg), argType, outType, fDef, body)
        }
  )
  
  def funcCall : Parser[Expr] = positioned(
    ident ~ ("(" ~> expr <~ ")") ^^ { case id ~ arg => Call(Var(id), arg) }
  )
   
  def typeName : Parser[Type] = positioned(
       complexType ~ "*" ~ typeName ^^
         { case left ~ "*" ~ right => ProdType(left, right) }
     | complexType ~ "+" ~ typeName ^^
         { case left ~ "+" ~ right => SumType(left, right) }
     | complexType
     )
     
  def complexType: Parser[Type] = positioned(
      simpleType ~ rep1("cont") ^^
         { case t ~ l => createContinuationType(t, l) }
     | simpleType
    )

  def simpleType : Parser[Type] = positioned(
       "num"  ^^^ { NumType }
     | "str"  ^^^ { StrType }
     | "unit" ^^^ { UnitType }
     | "void" ^^^ { VoidType }
     )

  def proj : Parser[Expr] = positioned(
      "prl" ~ "(" ~> expr <~ ")" ^^ { case exp => Projection(LeftProj, exp) }
    | "prr" ~ "(" ~> expr <~ ")" ^^ { case exp => Projection(RightProj, exp) }
    )
    
  def pair : Parser[Expr] = positioned(
      "<" ~ ">" ^^^ { Triv }
    |  ("<" ~> expr <~ ",") ~ (expr <~ ">") ^^ {
        case left ~ right => Pair(left, right)
      }
    )

  def sumExpr : Parser[Expr] = positioned(
      "abort" ~ "(" ~> typeName ~ "," ~ expr <~ ")" ^^ {
          case typ ~ "," ~ exp => Abort(typ, exp)
        }
    | inl ~ "(" ~> expr <~ ")" ^^ { case exp => InExpr(LeftProj, exp) }
    | inr ~ "(" ~> expr <~ ")" ^^ { case exp => InExpr(RightProj, exp) }
    | ("case" ~> expr <~ "{") ~
        (inl ~ "(" ~> ident <~ ")" ~ "=>") ~
        (expr <~ "|") ~
        (inr ~ "(" ~> ident <~ ")" ~ "=>") ~
        (expr <~ "}") ^^
        { case cond ~ v1 ~ body1 ~ v2 ~ body2 =>
              Case(cond, Var(v1), body1, Var(v2), body2)
        }
    )

  def matchExpr : Parser[Expr] = positioned(
    ("match" ~> expr <~ "{") ~ (rules <~ "}") ^^ { case exp ~ rs => MatchExpr(exp, rs) }
  )
      
  def rules : Parser[List[Rule]] = repsep(singleRule, "|")

  def singleRule : Parser[Rule] = positioned(
    (varList <~ ".") ~ (pattern <~ "=>") ~ expr ^^ {
        case vars ~ p ~ e => Rule(vars, p, e)
      }
  )

  def varList : Parser[List[Var]] =
    repsep(ident, ",") ^^ { l:List[String] => l map(id => Var(id)) }

  def pattern : Parser[Pattern] = positioned(
      "_" ^^^ { WildPattern }
    | ident ^^ { Var(_) }
    | "<" ~ ">" ^^^ { Triv }
    | ("<" ~> pattern <~ ",") ~ (pattern <~ ">") ^^ {
        case left ~ right => PairPattern(left, right)
      }
    | inl ~ "(" ~> pattern <~ ")" ^^ { case pat => InPattern(LeftProj, pat) }
    | inr ~ "(" ~> pattern <~ ")" ^^ { case pat => InPattern(RightProj, pat) }
    )

  def inl = "in" ~ "[" ~ elem(lexical.Identifier("l")) ~ "]" 
  
  def inr = "in" ~ "[" ~ elem(lexical.Identifier("r")) ~ "]" 

  // helper methods

  def parse(s: lexical.Scanner) = phrase(expr)(s)
  
  def getTokens(s: lexical.Scanner): List[lexical.Token] = {
    if (s.atEnd) s.first :: Nil
    else s.first :: getTokens(s.rest)
  }
  
  private def createContinuationType(t: Type, l: List[String]):Type = l match {
    case Nil => t
    case x::xs => createContinuationType(ContinuationType(t), xs)
  }
}
