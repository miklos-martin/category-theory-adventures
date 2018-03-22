package category
package instances

trait ListInstances {
  implicit val listFunctor = new Functor[List] {
    def fmap[A, B] = f => _.map(f)
  }
}
