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
  implicit val optionInstance = new Functor[Option] {
    def fmap[A, B] = f => _.map(f)
  }

  proveFunctorLaws[Option]
}

object ReaderExcercise extends Properties("Reader Functor") with FunctorLaws {
  implicit def reader[T] = new Functor[T => ?] {
    def fmap[A, B]: (A => B) => (T => A) => (T => B) =
      f => g => f compose g
  }

  proveFunctorLaws[String => ?]
}

object ConstExcercise extends Properties("Const functor") with FunctorLaws {
  case class Const[C, B](c: C)

  implicit def functor[C] = new Functor[Const[C, ?]] {
    def fmap[A, B] = _ => {
      case Const(c) => Const(c)
    }
  }

  import Arbitrary.arbitrary
  implicit def gen[C: Arbitrary, A]: Arbitrary[Const[C, A]] = Arbitrary(for (c <- arbitrary[C]) yield Const(c))

  proveFunctorLaws[Const[String, ?]]
}

object FunctorComposition extends Properties("Functor composition") with FunctorLaws {
  implicit val listF = new Functor[List] {
    def fmap[A, B] = f => _.map(f)
  }

  implicit val optionF = new Functor[Option] {
    def fmap[A, B] = f => _.map(f)
  }

  proveFunctorLaws[λ[A => Option[List[A]]]]
  proveFunctorLaws[λ[A => List[Option[A]]]]
}
