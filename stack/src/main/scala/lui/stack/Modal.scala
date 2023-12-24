package lui.stack
import com.raquo.laminar.api.L._
import com.raquo.laminar.api.L
import lui.util._
import lui.{_}

private[stack] case class ModalBuilder(
    child: Source[HtmlElement],
    title: Source[String],
    activeIn: Source[Boolean],
    activeOut: Sink[Boolean]
) extends Builder[Modal] {
  def build() = {
    val b = this
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

    new Modal(root)
  }

}
class Modal private[stack] (val root: HtmlElement) extends Component

object Modal extends Companion[Modal, ModalBuilder] {

  protected object keys extends ChildKey with ActiveInOutKey with TitleKey {
    protected type Builder = ModalBuilder
    protected type ChildValue = HtmlElement

    protected val activeKeyIn =
      mkIn((b, v) => b.copy(activeIn = v))
    protected val titleKey =
      mkIn((b, v) => b.copy(title = v))
    protected val childKey =
      mkIn((b, v) => b.copy(child = v))

    protected val activeKeyOut =
      mkOut((b, v) => b.copy(activeOut = v))

  }
  type X = keys.type
  val x = keys

  def empty = ModalBuilder(
    Signal.fromValue(div()),
    Signal.fromValue(""),
    Signal.fromValue(false),
    Observer.empty[Boolean]
  )

}
