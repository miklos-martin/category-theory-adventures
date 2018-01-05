package category

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll

object FunctorLaws extends Properties("Functor") with ArrowProperties {

  implicit val optionInstance = new Functor[Option] {
    def fmap[A, B](f: A => B): Option[A] => Option[B] = {
      case None => None
      case Some(a) => Some(f(a))
    }
  }

  def fmap[A, B](f: A => B) = Functor[Option].fmap(f)

  property("preserves id") = arrowIsIdentity(fmap[String, String](identity))

  val f: Int => Boolean = _ > 5
  val g: String => Int = _.length
  property("preserves composition") = arrowsEqual(fmap(f compose g), fmap(f) compose fmap(g))
}

object ReaderExcercise extends Properties("Reader") {

  implicit def reader[T] = new Functor[T => ?] {
    def fmap[A, B](f: A => B): (T => A) => (T => B) = f compose _
  }

  property("preserves id") = forAll { (f: String => Int, s: String) =>
    val mappedF = Functor[String => ?].fmap[Int, Int](identity)(f)
    f(s) == mappedF(s)
  }

  property("preserves composition") = forAll { (a: String => String, f: Int => Boolean, g: String => Int, s: String) =>
    def fmap[A, B](f: A => B) = Functor[String => ?].fmap(f)

    val arrow1 = fmap(f compose g)
    val arrow2 = fmap(f) compose fmap(g)

    arrow1(a)(s) == arrow2(a)(s)
  }
}
