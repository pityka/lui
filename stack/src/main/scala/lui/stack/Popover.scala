package lui.stack
import com.raquo.laminar.api.L._
import com.raquo.laminar.api.L
import lui.util._
import lui.{util, _}
import stack.{KeyTypes => K}

object Popover {
  /** Popover's parent element must be position: relative
    */
  def apply(mods: util.Mod[Popover.Param]*) =
    Popover.fromParam(
      util
        .build(Popover.Param.empty)(mods: _*)
    )
  case class Param(
      child: Source[HtmlElement],
      hasClose: Source[Boolean],
      activeIn: Source[Boolean],
      activeOut: Sink[Boolean]
  )
  object Param {
    def empty = Param(
      Signal.fromValue(div()),
      Signal.fromValue(false),
      Signal.fromValue(false),
      Observer.empty[Boolean]
    )

  }
  case class Component(root: HtmlElement)
  object Component {
    import scala.language.implicitConversions
    implicit def conv(c: Component): HtmlElement = c.root
  }
  private type In[K, V] = Key[K, Param, Source[V]]
  private type Out[K, V] = Key[K, Param, Sink[V]]

  implicit val assignChild: In[K.child, HtmlElement] =
    mk((b, v) => b.copy(child = v))
  implicit val assignHasClose: In[K.withCloseButton, Boolean] =
    mk((b, v) => b.copy(hasClose = v))
  implicit val assignIn: In[K.active, Boolean] =
    mk((b, v) => b.copy(activeIn = v))
  implicit val assignOut: Out[K.value, Boolean] =
    mk((b, v) => b.copy(activeOut = v))

  def fromParam(b: Param) = {

    val state = Var(false)

    val effectiveState = state.signal.changes.mergeWith(
      b.activeIn.toObservable.toWeakSignal
        .map(_.getOrElse(false))
        .changes
    )

    val containerStyle = effectiveState.map{ open =>
      if (open) "s-popover is-visible" 
      else "s-popover"      
    }

    val root = div(
      cls <-- containerStyle,
      div(
        L.child <-- b.child,
        L.child <-- b.hasClose.toObservable.map{ bool => if (bool) span(button(
          typ := "button",
          cls := "s-popover--close s-btn s-btn__muted",
          svg.svg(
            svg.cls := "svg-icon iconClearSm",
            svg.width := "14",
            svg.height := "14",
            svg.viewBox := "0 0 14 14",
            svg.path(
              svg.d := "M12 3.41 10.59 2 7 5.59 3.41 2 2 3.41 5.59 7 2 10.59 3.41 12 7 8.41 10.59 12 12 10.59 8.41 7 12 3.41Z"
            )
          ),
          onClick.mapToUnit.map(_ => false) --> state.writer
        )) else span()}
      )
    )

    Component(root)
  }

}
