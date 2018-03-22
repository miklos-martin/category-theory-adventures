package category
package instances

trait Tuple2Instances {
  implicit val tuple2Bifunctor = new Bifunctor[Tuple2] {
    override def bimap[A, T, B, U] = f => g => {
      case (a, b) => (f(a), g(b))
    }
  }
}
