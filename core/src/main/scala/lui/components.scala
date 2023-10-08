package lui
import com.raquo.laminar.api.L.{Sink, Signal, Source, Modifier, HtmlElement}
import util._
 trait Comp extends Modifier[HtmlElement] {
    def root : HtmlElement
    override def apply(element: HtmlElement): Unit = root(element)
  }
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




