package category
package instances

trait OptionInstances {
  implicit val optionFunctor = new Functor[Option] {
    def fmap[A, B] = f => _.map(f)
  }
}
