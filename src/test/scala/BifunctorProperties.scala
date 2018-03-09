package category

import org.scalacheck.{Arbitrary, Gen, Properties}
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
  import EitherAsBifunctor.eitherInstance
  import TupleAsBifunctor.tupleInstance

  proveBifunctorLaws[λ[(A, B) => Either[(A, B), (A, B)]]]
  proveBifunctorLaws[λ[(A, B) => (Either[A, B], Either[A, B])]]
}

object BiCompExcercise extends Properties("BiComp") with BifunctorLaws {
  case class BiComp[BF[_, _], F[_], G[_], A, B](x: BF[F[A], G[B]])

  implicit def bicompInstance[BF[_, _]: Bifunctor, F[_]: Functor, G[_]: Functor] = new Bifunctor[BiComp[BF, F, G, ?, ?]] {
    override def bimap[A, T, B, U] = f => g => {
      case BiComp(x) => BiComp(Bifunctor[BF].bimap(Functor[F].fmap(f))(Functor[G].fmap(g))(x))
    }
  }

  import TupleAsBifunctor.tupleInstance
  implicit val optionFunctor = new Functor[Option] {
    def fmap[A, B] = f => _.map(f)
  }
  implicit val listFunctor = new Functor[List] {
    def fmap[A, B] = f => _.map(f)
  }

  implicit def genBiComp[A: Arbitrary, B: Arbitrary]: Arbitrary[BiComp[Tuple2, Option, List, A, B]] =
    Arbitrary(for(x <- arbitrary[(Option[A], List[B])]) yield BiComp(x))

  proveBifunctorLaws[BiComp[Tuple2, Option, List, ?, ?]]
}
