package category

import org.scalacheck.{Arbitrary, Properties}
import org.scalacheck.Prop.forAll
import Isomorphism._

import scala.reflect.runtime.universe._

trait IsomorphicProperties { self: Properties =>
  def arrowsEqual[A : Arbitrary, B](f: A => B, g: A => B) = forAll { a: A =>
    f(a) == g(a)
  }

  def arrowIsIdentity[A : Arbitrary](f: A => A) = arrowsEqual[A, A](f, identity)

  def proveTypesAreIsoMorphic[A : Arbitrary : TypeTag, B : TypeTag](implicit ev: A <=> B) = {
    property(s"isomorphism roundtrip == identity :: ${typeTag[A].tpe} <=> ${typeTag[B].tpe}") = arrowIsIdentity(ev.a2b andThen ev.b2a)
  }
}

object IsomorphicProps extends Properties("Isomorphism") with IsomorphicProperties {
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

  property("isomorphism with self: =>") = arrowIsIdentity(Isomorphism[String].a2b)
  property("isomorphism with self: <=") = arrowIsIdentity(Isomorphism[String].b2a)

  property("reversibility") = forAll { (value: Either[Unit, String]) =>
    import Isomorphism.syntax._
    value.as[Option[String]].as[Either[Unit, String]] == value
  }
}
