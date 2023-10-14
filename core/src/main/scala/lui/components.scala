package lui
import com.raquo.laminar.api.L.{Sink, Signal, Source, Modifier, HtmlElement}
import util._

class InSyntax[K] {
  def :=[B, V](v: V)(implicit ev: Key[K, B, Source[V]]): Mod[B] = ev(
    Signal.fromValue(v)
  )
  def <--[B, V](v: Source[V])(implicit
      ev: Key[K, B, Source[V]]
  ): Mod[B] = ev(v)
}
class OutSyntax[K] {
  def -->[B, V](v: Sink[V])(implicit ev: Key[K, B, Sink[V]]): Mod[B] =
    ev(v)
}

trait Key[K, B, V] {
  def apply(v: V): B => B
}

trait Comp extends Component

trait Component extends Modifier[HtmlElement] {
  def root: HtmlElement
  override def apply(element: HtmlElement): Unit = root(element)
}

trait Builder[C<:Component] {
  def build() : C 
}

trait Companion[C<:Component, B<:Builder[C]] {
  type X
  val x: X
  def empty: B 
  def apply(mods: X => util.Mod[B]*): C =
    util
      .build(empty)(mods.map(_(x)): _*).build()

  protected type In[K, V] = Key[K, B, Source[V]]
  protected type Out[K, V] = Key[K,B, Sink[V]]
}
