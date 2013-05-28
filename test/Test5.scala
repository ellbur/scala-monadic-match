
import scalaz._
import Scalaz._
import ellbur.monadicmatch._

/**
 * Demonstrating use of conditions.
 */
object Test5 {
  val mm = new MonadicMatch {
    type M[+X] = List[X]
    val monad = implicitly[Monad[List]]
  }
  import mm._

  def main(args: Array[String]) {
    println{
      List(1, 2, 3, 4) mmatch (
        S when (_ >= 2)
      )
    }
  }
}
