
import scalaz._
import Scalaz._
import ellbur.monadicmatch._

object Test2 {
  val mm = new MonadicMatch {
    type M[+X] = List[X]
    val monad = implicitly[Monad[List]]
  }
  import mm._

  sealed trait AOrB
  case class A(xs: List[Int]) extends AOrB
  case class B(xs: List[Int]) extends AOrB

  object extractA extends Extractor[AOrB, List[Int]] {
    def extract(f: AOrB) = f match {
      case A(xs) => some(xs).pure[List]
      case B(_) => None.pure[List]
    }
  }

  def main(args: Array[String]) {
    val it = A(1 :: 2 :: 3 :: Nil)

    println {
      {
        extractA(S)
      }.extract(it)
    }
  }
}
