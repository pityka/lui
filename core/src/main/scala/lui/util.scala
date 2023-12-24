package lui
import com.raquo.laminar.api.L.{Sink, Source}

private[lui] object util {
  type Mod[B] = B => B

  type SourceKey[T, B, V] = Key[T, B, Source[V]]
  type SinkKey[T, B, V] = Key[T, B, Sink[V]]

  def mk[T, B, V](f: (B,V) => B) = new Key[T, B, V] {
    def apply(v: V): B => B = (b) => f(b, v)
  }
  def mkIn[T, B, V](f: (B,Source[V]) => B) = new InKey[T, B, V] {
    def apply(v: Source[V]): B => B = (b) => f(b, v)
  }
  def mkOut[T, B, V](f: (B,Sink[V]) => B) = new OutKey[T, B, V] {
    def apply(v: Sink[V]): B => B = (b) => f(b, v)
  }
  


  def build[B](init: B)(mods: Mod[B]*): B = mods.foldLeft(
    init
  )((a, b) => b apply a)
}
