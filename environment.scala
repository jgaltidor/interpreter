package mylang
import scala.collection.mutable.HashMap
import java.lang.String.format
import Utils._

class ScopedEnvironment[B](var outerEnv : ScopedEnvironment[B]) extends HashMap[Var, B]
{
	def this() = this(null)

	override def apply(key: Var):B = get(key) match {
		case Some(v) => v
		case None =>
			if(outerEnv != null) outerEnv(key)
			else throw new NotInScope(key)
	}

	def containsInScope(key: Var):Boolean =
		if(contains(key)) return true
		else return false
}
