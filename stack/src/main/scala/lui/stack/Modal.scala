package lui.stack
import com.raquo.laminar.api.L._
import com.raquo.laminar.api.L
import lui.util._
import lui.{util, _}
import stack.{KeyTypes => K}

object Modal {
  def apply(mods: util.Mod[Modal.Param]*) =
    Modal.fromParam(
      util
        .build(Modal.Param.empty)(mods: _*)
    )
  case class Param(
      child: Source[HtmlElement],
      title: Source[String],
      activeIn: Source[Boolean],
      activeOut: Sink[Boolean]
  )
  object Param {
    def empty = Param(
      Signal.fromValue(div()),
      Signal.fromValue(""),
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
  implicit val assignTitle: In[K.title, String] =
    mk((b, v) => b.copy(title = v))
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

    val root = div(
      cls := "s-modal",
      aria.hidden <-- effectiveState.map(b => !b),
      div(
        cls := "s-modal--dialog",
        h1(cls := "s-modal--header", L.child.text <-- b.title),
        div(cls := "s-modal-body wmn2", L.child <-- b.child),
        button(
          typ := "button",
          cls := "s-modal--close s-btn s-btn__muted",
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
        )
      )
    )

    Component(root)
  }

}
