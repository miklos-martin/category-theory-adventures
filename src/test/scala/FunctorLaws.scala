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
    def fmap[A, B](f: A => B): Option[A] => Option[B] = {
      case None => None
      case Some(a) => Some(f(a))
    }
  }

  proveFunctorLaws[Option]
}

object ReaderExcercise extends Properties("Reader Functor") with FunctorLaws {
  implicit def reader[T] = new Functor[T => ?] {
    def fmap[A, B](f: A => B): (T => A) => (T => B) = f compose _
  }

  proveFunctorLaws[String => ?]
}

object ConstExcercise extends Properties("Const functor") with FunctorLaws {
  case class Const[C, B](c: C)

  implicit def functor[C] = new Functor[Const[C, ?]] {
    def fmap[A, B](f: A => B) = {
      case Const(c) => Const(c)
    }
  }

  import Arbitrary.arbitrary
  implicit def gen[C: Arbitrary, A]: Arbitrary[Const[C, A]] = Arbitrary(for (c <- arbitrary[C]) yield Const(c))

  proveFunctorLaws[Const[String, ?]]
}
