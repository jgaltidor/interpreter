package mylang
import MyParser._
import Utils._
import scala.util.parsing.input._
import java.io.{BufferedReader, FileReader}

object Tester
{
  def test(progstr: String):Unit = test(new lexical.Scanner(progstr))
  
  def test(in: Reader[Char]):Unit = test(new lexical.Scanner(in))

  def test(scan: lexical.Scanner):Unit = {
      println(getTokens(scan) mkString " ")
      parse(scan) match {
        case Success(tree, _) =>
          println("Tree: "+tree)
          
          println("Type checking AST")
          TypeChecker.typchk(tree) match {
            case TypeChecker.Success(t) =>
              TypeChecker.printtype(tree)
              println()

              println("program evaluates to: " + colorstr(Evaluator.eval(tree)))
            case TypeChecker.Failure(exc) =>
              TypeChecker.printtype(tree)
              println()
              println(exc)
          }
        case e: NoSuccess => Console.err.println(e)
      }
  }

  //A main method for testing
  def main(args: Array[String]) =
    test(StreamReader(new BufferedReader(new FileReader(args(0)))))
}

object Interpreter
{
  def main(args: Array[String]) = {
    if(args.length == 0) {
      Console.err.println("usage: scala mylang.Interpreter <file1> ... <fileN>")
      exit(1)
    }
    for(arg <- args) interpretFile(arg)
  }
  
  def interpretFile(filename: String) = {
    val in:Reader[Char] = StreamReader(new BufferedReader(new FileReader(filename)))
    println("Interpreting " + filename)
    // println("Parsing " + filename)
    parse(new lexical.Scanner(in)) match {
      case Success(tree, _) =>
        // println("Typechecking " + filename)
        TypeChecker.typchk(tree) match {
          case TypeChecker.Success(t) =>
            // TypeChecker.printtype(tree); println()
            // println("Evaluating " + filename)
            Evaluator.eval(tree) match {
              case err:ErrorVal =>
                println(colorstr("Runtime Error: ", RED) +
                "From " + filename + ":" + err.errMsg)
              case v:Value =>
                println("program evaluates to: " + colorstr(v, GREEN))
            }
          case TypeChecker.Failure(exc) =>
            // TypeChecker.printtype(tree); println()
            Console.err.println(filename + ":" + exc.msg)
        }
      case e: NoSuccess =>
        Console.err.println("Parse error in " + filename)
        Console.err.println(e)
    }
  }
}
