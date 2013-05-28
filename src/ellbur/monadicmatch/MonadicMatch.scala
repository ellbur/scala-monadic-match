
package ellbur.monadicmatch

import scalaz._
import Scalaz._

trait MonadicMatch {
  type M[+X]
  implicit val monad: Monad[M]

  trait Extractor[-From,+To] { self =>
    def extract(x: From): M[Option[To]]

    def apply[Next](e: Extractor[To,Next]): Extractor[From,Next] = new Extractor[From,Next] {
      def extract(x: From) = {
        val first = self.extract(x)
        first flatMap (f => (f map { first =>
          e.extract(first)
        }).sequence map (_.flatten))
      }
    }

    def /[Next](e: Extractor[To,Next]): Extractor[From,Next] = apply(e)

    def ~>[Next](f: To => Next) = new Extractor[From, Next] {
      def extract(x: From) = self.extract(x) map (_ map f)
    }

    def map[Next](f: To => Next) = this ~> f

    def zip[From2,To2](other: Extractor[From2,To2]) = pair(self, other)

    def |[From2<:From,To2>:To](other: Extractor[From2,To2]) = new Extractor[From2, To2] {
      def extract(x: From2) =
        self.extract(x) flatMap {
          case Some(y) => Some(y).pure[M]
          case None => other.extract(x)
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

  def Id[A] = new Extractor[A, A] {
    def extract(x: A): M[Option[A]] = Some(x).pure[M]
  }

  implicit class MMatch[From](x: From) {
    def mmatch[To](e: Extractor[From, To]) = e.extract(x)
  }

  def partial[A] = new {
    def apply[B](f: PartialFunction[A, B]) = new Extractor[A, B] {
      def extract(x: A) = f.lift(x).pure[M]
    }
  }

  def any = new {
    def apply[B](f: PartialFunction[AnyRef, B]) = new Extractor[AnyRef, B] {
      def extract(x: AnyRef) = f.lift(x).pure[M]
    }
  }

  def extract[From,By,To](x: From)(e: Extractor[From,By])(f: By => To): M[Option[To]] =
    e.extract(x) map (_ map f)

  trait Unapply[From, To] extends Extractor[From, To] {
    def unapply(x: From): Option[To]
    def extract(x: From): M[Option[To]] = unapply(x).pure[M]
  }
}
