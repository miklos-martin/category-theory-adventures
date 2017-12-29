package category

import org.scalacheck.{Arbitrary, Properties}
import org.scalacheck.Prop.forAll

object IsomorphicProperties extends Properties("Isomorphism") {
  import Isomorphism._
  import Isomorphism.syntax._

  type EitherUnit[A] = Either[Unit, A]

  implicit def evidence[A] = new Isomorphism[EitherUnit[A], Option[A]] {
    def a2b = {
      case Left(_) => None
      case Right(a) => Some(a)
    }

    def b2a = {
      case None => Left(())
      case Some(a) => Right(a)
    }
  }

  def arrowsEqual[A, B](f: A => B, g: A => B)(implicit gen: Arbitrary[A]) = forAll { a: A =>
    f(a) == g(a)
  }

  def arrowIsIdentity[A](f: A => A)(implicit gen: Arbitrary[A]) = arrowsEqual[A, A](f, identity)

  val ev = evidence[String]
  property("roundtrip == identity") = arrowIsIdentity(ev.a2b andThen ev.b2a)

  val selfIso = implicitly[String <=> String]
  property("isomorphism with self") = arrowIsIdentity(selfIso.a2b)

  property("reversibility") = forAll { (value: Either[Unit, String]) =>
    implicit val howcouldthisbeautomatic = reverse(evidence[String])

    value.as[Option[String]].as[Either[Unit, String]] == value
  }
}
