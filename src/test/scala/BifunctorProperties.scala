package category

import org.scalacheck.{Arbitrary, Properties}

trait BifunctorLaws extends FunctorLaws { self: Properties =>
  import Bifunctor._

  def proveBifunctorLaws[F[_, _]: Bifunctor](implicit ais: Arbitrary[F[Int, String]], asi: Arbitrary[F[String, Int]]) = {
    proveFunctorLaws[F[Int, ?]]
    proveFunctorLaws[F[?, Int]]
  }
}

object EitherAsBifunctor extends Properties("Bifunctor for Either") with BifunctorLaws {
  implicit val eitherInstance = new Bifunctor[Either] {
    def bimap[A, T, B, U](f: A => T)(g: B => U): Either[A, B] => Either[T, U] = {
      case Left(a) => Left(f(a))
      case Right(b) => Right(g(b))
    }
  }

  proveBifunctorLaws[Either]
}

object TupleAsBifunctor extends Properties("Bifunctor for Tuple2") with BifunctorLaws {
  implicit val tupleInstance = new Bifunctor[Tuple2] {
    def bimap[A, T, B, U](f: A => T)(g: B => U): ((A, B)) => (T, U) = {
      case (a, b) => (f(a), g(b))
    }
  }

  proveBifunctorLaws[Tuple2]
}
