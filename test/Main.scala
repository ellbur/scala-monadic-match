
import scalaz._
import Scalaz._

object Main extends App {
  trait MonadicMatch {
    type M[+X]
    implicit val monad: Monad[M]

    trait Extractor[From,To] { self =>
      def extract(x: From): M[Option[To]]
      def apply[Next](e: Extractor[To,Next]): Extractor[From,Next] = new Extractor[From,Next] {
        def extract(x: From) = {
          val first = self.extract(x)
          first flatMap (f => (f map { first =>
            e.extract(first)
          }).sequence map (_.flatten))
        }
      }
    }

    def pair[A1,A2,B1,B2](e1: Extractor[A1,B1], e2: Extractor[A2,B2]) = new Extractor[(A1,A2),(B1,B2)] {
      def extract(x: (A1, A2)) = {
        val (a1, a2) = x
        val b1 = e1.extract(a1)
        val b2 = e2.extract(a2)

        (b1 |@| b2) { (b1, b2) =>
          (b1 |@| b2)((_,_))
        }
      }
    }

    def S[A] = new Extractor[M[A],A] {
      def extract(x: M[A]): M[Option[A]] = x map (Some(_))
    }

    case class SomeE[A]() extends Extractor[Option[A],A] {
      def extract(x: Option[A]): M[Option[A]] = x.pure[M]
    }
    def SE[A] = SomeE[A]()

    def Id[A] = new Extractor[A, A] {
      def extract(x: A): M[Option[A]] = Some(x).pure[M]
    }

    def extract[From,By,To](x: From)(e: Extractor[From,By])(f: By => To): M[Option[To]] =
      e.extract(x) map (_ map f)

    def valueOf(x: Option[Option[Int]]): M[Option[Int]] =
      extract(x) ( SE(SE) ) { v =>
        v
      }

    def valueOf2(x: Option[(Option[Int], Int)]): M[Option[(Int, Int)]] =
      extract(x) ( SE(pair(SE, Id)) ) { case (a, b) =>
        (a, b)
      }

    def valueOf3(x: Option[M[Option[Int]]]): M[Option[Int]] =
      extract(x) ( SE(S(SE)) ) { v => v }
  }

  val mm = new MonadicMatch {
    type M[+X] = List[X]
    val monad = implicitly[Monad[List]]
  }
  import mm._

  println {
    valueOf3(Some(List(None, Some(1), None, Some(2))))
  }
}
