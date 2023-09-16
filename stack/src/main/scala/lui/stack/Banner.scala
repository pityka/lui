package lui.stack
import com.raquo.laminar.api.L._
import com.raquo.laminar.api.L
import lui.util._
import lui.{util, _}
import stack.{KeyTypes => K}
object Banner {
  def apply(mods: util.Mod[Banner.Param]*): Component =
    Banner.fromParam(
      util
        .build(Banner.Param.empty)(mods: _*)
    )
  case class Param(
      child: Source[HtmlElement],
      variant: Source[Variant],
      activeIn: Source[Boolean],
      activeOut: Sink[Boolean]
  )
  object Param {
    def empty = Param(
      Signal.fromValue(div()),
      Signal.fromValue(Info),
      Signal.fromValue(false),
      Observer.empty[Boolean]
    )

  }

  sealed trait Variant
  private[Banner] object Variant {
    case object Info extends Variant
    case object InfoImportant extends Variant
    case object Success extends Variant
    case object SuccessImportant extends Variant
    case object Warning extends Variant
    case object WarningImportant extends Variant
    case object Danger extends Variant
    case object DangerImportant extends Variant

  }
  val Info: Variant = Variant.Info
  val Success: Variant = Variant.Success
  val Warning: Variant = Variant.Warning
  val Danger: Variant = Variant.Danger
  val InfoImportant: Variant = Variant.InfoImportant
  val SuccessImportant: Variant = Variant.SuccessImportant
  val WarningImportant: Variant = Variant.WarningImportant
  val DangerImportant: Variant = Variant.DangerImportant

  case class Component(root: HtmlElement)
  object Component {
    import scala.language.implicitConversions
    implicit def conv(c: Component): HtmlElement = c.root
  }
  private type In[K, V] = Key[K, Param, Source[V]]
  private type Out[K, V] = Key[K, Param, Sink[V]]

  implicit val assignChild: In[K.child, HtmlElement] =
    mk((b, v) => b.copy(child = v))
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
      cls <-- b.variant.toObservable.map { variant =>
        val base = " s-banner"
        (variant match {
          case Variant.Info          => "s-banner__info"
          case Variant.InfoImportant => "s-banner__danger s-banner__important"
          case Variant.Success       => "s-banner__success"
          case Variant.SuccessImportant =>
            "s-banner__danger s-banner__important"
          case Variant.Warning => "s-banner__warning"
          case Variant.WarningImportant =>
            "s-banner__danger s-banner__important"
          case Variant.Danger          => "s-banner__danger"
          case Variant.DangerImportant => "s-banner__danger s-banner__important"
        }) + base
      },
      aria.hidden <-- effectiveState.map(b => !b),
      div(
        cls := "d-flex flex__center s-banner__container",
        div(cls := "g8", L.child <-- b.child),
        div(
          cls := "ml-auto myn8",
          button(
            typ := "button",
            cls := "s-banner__btn s-btn s-btn__muted",
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
    )

    Component(root)
  }

}
