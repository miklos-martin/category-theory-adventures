package category

import org.scalacheck.{Arbitrary, Properties}

trait FunctorLaws extends ArrowProperties { self: Properties =>
  def proveFunctorLaws[F[_] : Functor](implicit ev: Arbitrary[F[String]], evEqString: Eq[F[String]], evEqBoolean: Eq[F[Boolean]]) = {
    def fmap[A, B](f: A => B) = Functor[F].fmap(f)

    property("preserves id") = arrowIsIdentity(fmap[String, String](identity))

    val f: Int => Boolean = _ > 5
    val g: String => Int = _.length
    property("preserves composition") = arrowsEqual(fmap(f compose g), fmap(f) compose fmap(g))
  }
}

object OptionFunctor extends Properties("Functor for Option") with FunctorLaws {
  import instances.option._
  proveFunctorLaws[Option]
}

object ListFunctor extends Properties("Functor for List") with FunctorLaws {
  import instances.list._
  proveFunctorLaws[List]
}

object ReaderExcercise extends Properties("Reader Functor") with FunctorLaws {
  implicit def reader[T] = new Functor[T => ?] {
    def fmap[A, B]: (A => B) => (T => A) => (T => B) =
      f => g => f compose g
  }

  proveFunctorLaws[String => ?]
}

object FunctorComposition extends Properties("Functor composition") with FunctorLaws {
  import instances.option._
  import instances.list._

  proveFunctorLaws[λ[A => Option[List[A]]]]
  proveFunctorLaws[λ[A => List[Option[A]]]]
}
