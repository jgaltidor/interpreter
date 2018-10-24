package mylang

object Utils
{
  abstract sealed class ANSIColor(colorCode: String) {
    val colorCodeStr = colorCode; 
  }
  case object BLUE   extends ANSIColor("\033[1;34m")
  case object PURPLE extends ANSIColor("\033[1;35m")
  case object GREEN  extends ANSIColor("\033[1;32m")
  case object YELLOW extends ANSIColor("\033[1;33m")
  case object RED    extends ANSIColor("\033[1;31m")

  private val ansiReset  = "\033[1;0m"

  def colorstr(s:AnyRef):String = colorstr(s, GREEN)
  def colorstr(s:AnyRef, c:ANSIColor):String = c.colorCodeStr + s + ansiReset
  def colorprint(s:AnyRef) = print(colorstr(s))
  def colorprintln(s:AnyRef) = println(colorstr(s))  
  
  def getVarNames(p: Pattern): List[String] = p match {
      case v:Var => v.name::Nil
      case PairPattern(p1, p2) => getVarNames(p1) ++ getVarNames(p2)
      case InPattern(index, p) => getVarNames(p)
      case _ => Nil
    }
    
  def getSharedElement[A](l1: List[A], l2: List[A]):Option[A] = l1 match {
      case Nil => None
      case x::xs =>
        if(l2 contains x) Some(x)
        else getSharedElement(xs, l2)
    }

  def getDuplicate[A](l: List[A]):Option[A] = l match {
    case Nil => None
    case x::xs =>
      if(xs contains x) Some(x)
      else getDuplicate(xs)
  }

  def getSharedElement[A](itr1: Iterator[A], itr2: Iterator[A]):Option[A] =
    getSharedElement(itr1.toList, itr2.toList)  
}


