package lui
import com.raquo.laminar.api.L.{Sink, Signal, Source, Modifier, HtmlElement}
import util._

class InSyntax[K, B, V](ev: InKey[K, B, V]) {
  def :=(v: V): Mod[B] = ev(
    Signal.fromValue(v)
  )
  def <--(v: Source[V]): Mod[B] = ev(v)
}
class OutSyntax[K, B, V](ev: OutKey[K, B, V]) {
  def -->(v: Sink[V]): Mod[B] = ev(v)
}
class InOutSyntax[K, B, V](evIn: InKey[K, B, V], evOut: OutKey[K, B, V]) {
  def :=(v: V): Mod[B] = evIn(
    Signal.fromValue(v)
  )
  def <--(v: Source[V]): Mod[B] = evIn(v)

  def -->(v: Sink[V]): Mod[B] = evOut(v)
}

trait Key[K, B, V] {
  def apply(v: V): B => B
}
trait InKey[K, B, V] extends Key[K, B, Source[V]] {
  def apply(v: Source[V]): B => B
}
trait OutKey[K, B, V] extends Key[K, B, Sink[V]] {
  def apply(v: Sink[V]): B => B
}

trait Comp extends Component

trait Component extends Modifier[HtmlElement] {
  protected def root: HtmlElement
  override def apply(element: HtmlElement): Unit = root(element)
}

trait Builder[C <: Component] {
  def build(): C
}

trait Companion[C <: Component, B <: Builder[C]] {
  protected type X
  protected val x: X
  def empty: B
  def apply(mods: X => util.Mod[B]*): C =
    util
      .build(empty)(mods.map(_(x)): _*)
      .build()

  protected type In[K, V] = InKey[K, B, V]
  protected type Out[K, V] = OutKey[K, B, V]
}
