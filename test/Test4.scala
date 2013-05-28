
import scalaz._
import Scalaz._
import ellbur.monadicmatch._

/**
 * Shows how you can take an unapply() like extractor and use it as a monadic extractor.
 */
object Test4 {
  val mm = new MonadicMatch {
    type M[+X] = List[X]
    val monad = implicitly[Monad[List]]
  }
  import mm._

  sealed trait AOrB
  case class A(xs: List[Int]) extends AOrB
  case class B(xs: List[Int]) extends AOrB

  object eA extends Unapply[AOrB, List[Int]] {
    def unapply(ab: AOrB) = ab match {
      case A(xs) => Some(xs)
      case _ => None
    }
  }

  def main(args: Array[String]) {
    println {
      A(1 :: 2 :: 3 :: Nil) mmatch (
        eA(S) ~> (n => s"The number $n")
      )
    }
  }
}