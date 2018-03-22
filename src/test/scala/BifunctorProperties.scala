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
  import instances.either._

  proveBifunctorLaws[Either]
}

object TupleAsBifunctor extends Properties("Bifunctor for Tuple2") with BifunctorLaws {
  import instances.tuple2._

  proveBifunctorLaws[Tuple2]
}

object BifunctorDefinedInTermsOfFirstAndSecond extends Properties("Bifunctor defined in terms of first and second") with BifunctorLaws {
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

object BifunctorComposition extends Properties("Composition of two bifunctors") with BifunctorLaws {
  import instances.tuple2._
  import instances.either._

  proveBifunctorLaws[λ[(A, B) => Either[(A, B), (A, B)]]]
  proveBifunctorLaws[λ[(A, B) => (Either[A, B], Either[A, B])]]
}
