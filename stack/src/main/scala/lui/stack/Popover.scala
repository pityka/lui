package lui.stack
import com.raquo.laminar.api.L._
import com.raquo.laminar.api.L
import lui.util._
import lui.{_}

private[stack] case class PopoverBuilder(
    child: Source[HtmlElement],
    hasClose: Source[Boolean],
    activeIn: Source[Boolean],
    activeOut: Sink[Boolean]
) extends Builder[Popover] {
  def build(): Popover = {
    val b = this
    val state = Var(false)

    val effectiveState = state.signal.changes.mergeWith(
      b.activeIn.toObservable.toWeakSignal
        .map(_.getOrElse(false))
        .changes
    )

    val containerStyle = effectiveState.map { open =>
      if (open) "s-popover is-visible"
      else "s-popover"
    }

    val root = div(
      cls <-- containerStyle,
      div(
        L.child <-- b.child,
        L.child <-- b.hasClose.toObservable.map { bool =>
          if (bool)
            span(
              button(
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
              )
            )
          else span()
        }
      )
    )

    new Popover(root)
  }
}

class Popover private[stack] (val root: HtmlElement) extends Comp

/** Popover's parent element must be position: relative
  */
object Popover extends Companion[Popover, PopoverBuilder] {
  protected object keys
      extends ChildKey
      with WithCloseButtonKey
      with ActiveInOutKey {
    protected type Builder = PopoverBuilder
    protected type ChildValue = HtmlElement

    protected val activeKeyIn =
      mkIn((b, v) => b.copy(activeIn = v))
    protected val withCloseButtonKey =
      mkIn((b, v) => b.copy(hasClose = v))
    protected val childKey =
      mkIn((b, v) => b.copy(child = v))

    protected val activeKeyOut =
      mkOut((b, v) => b.copy(activeOut = v))

  }
  type X = keys.type
  val x = keys

  def empty = PopoverBuilder(
    Signal.fromValue(div()),
    Signal.fromValue(false),
    Signal.fromValue(false),
    Observer.empty[Boolean]
  )

}
