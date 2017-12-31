package category

import org.scalacheck.{Arbitrary, Properties}
import org.scalacheck.Prop.forAll
import Isomorphism._

trait IsomorphicProperties { self: Properties =>
  def arrowsEqual[A : Arbitrary, B](f: A => B, g: A => B) = forAll { a: A =>
    f(a) == g(a)
  }

  def arrowIsIdentity[A : Arbitrary](f: A => A) = arrowsEqual[A, A](f, identity)

  def proveTypesAreIsoMorphic[A : Arbitrary, B](implicit ev: Isomorphism[A, B]) = {
    property("isomorphism roundtrip == identity") = arrowIsIdentity(ev.a2b andThen ev.b2a)
  }
}

object IsomorphicProperties extends Properties("Isomorphism") with IsomorphicProperties {
  implicit def evidence[A] = new Isomorphism[Either[Unit, A], Option[A]] {
    def a2b = {
      case Left(_) => None
      case Right(a) => Some(a)
    }

    def b2a = {
      case None => Left(())
      case Some(a) => Right(a)
    }
  }

  proveTypesAreIsoMorphic[Either[Unit, String], Option[String]]

  val selfIso = implicitly[String <=> String]
  property("isomorphism with self") = arrowIsIdentity(selfIso.a2b)

  property("reversibility") = forAll { (value: Either[Unit, String]) =>
    import Isomorphism.syntax._
    implicit val howcouldthisbeautomatic = reverse(evidence[String])

    value.as[Option[String]].as[Either[Unit, String]] == value
  }
}
