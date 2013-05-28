
import scalaz._
import Scalaz._
import ellbur.monadicmatch._

object Test1 {
  val mm = new MonadicMatch {
    type M[+X] = List[X]
    val monad = implicitly[Monad[List]]
  }
  import mm._

  sealed trait AOrB
  case class A(xs: List[Int]) extends AOrB
  case class B(xs: List[Int]) extends AOrB

  object extractA extends Extractor[AOrB, Int] {
    def extract(f: AOrB) = f match {
      case A(xs) => xs map (Some(_))
      case B(_) => None.pure[List]
    }
  }

  def main(args: Array[String]) {
    println(extractA.extract(A(1 :: 2 :: 3 :: Nil)))
    println(extractA.extract(B(1 :: 2 :: 3 :: Nil)))
  }
}
