package category

import org.scalacheck.{Arbitrary, Properties}
import Arbitrary.arbitrary

trait BifunctorLaws extends FunctorLaws { self: Properties =>
  import Bifunctor._

  def proveBifunctorLaws[F[_, _]: Bifunctor](implicit ais: Arbitrary[F[Int, String]], asi: Arbitrary[F[String, Int]]) = {
    proveFunctorLaws[F[Int, ?]]
    proveFunctorLaws[F[?, Int]]
  }
}

object EitherAsBifunctor extends Properties("Bifunctor for Either") with BifunctorLaws {
  implicit val eitherInstance = new Bifunctor[Either] {
    override def bimap[A, T, B, U] = f => g => {
      case Left(a) => Left(f(a))
      case Right(b) => Right(g(b))
    }
  }

  proveBifunctorLaws[Either]
}

object TupleAsBifunctor extends Properties("Bifunctor for Tuple2") with BifunctorLaws {
  implicit val tupleInstance = new Bifunctor[Tuple2] {
    override def bimap[A, T, B, U] = f => g => {
      case (a, b) => (f(a), g(b))
    }
  }

  proveBifunctorLaws[Tuple2]
}

object BiFunctorDefinedInTermsOfFirstAndSecond extends Properties("Bifunctor defined in terms of first and second") with BifunctorLaws {
  case class Foo[A, B](a: A, b: B)
  implicit val fooInstance = new Bifunctor[Foo] {
    override def first[A, B, C] = f => foo => foo.copy(a = f(foo.a))
    override def second[A, B, D] = g => foo => foo.copy(b = g(foo.b))
  }

  implicit def fooGen[A: Arbitrary, B: Arbitrary]: Arbitrary[Foo[A, B]] =
    Arbitrary(for {
      a <- arbitrary[A]
      b <- arbitrary[B]
    } yield Foo(a, b))

  proveBifunctorLaws[Foo]
}
