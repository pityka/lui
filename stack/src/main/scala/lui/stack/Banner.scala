package lui.stack
import com.raquo.laminar.api.L._
import com.raquo.laminar.api.L
import lui.util._
import lui._
import stack.{KeyTypes => K}
class Banner private[stack] (val root: HtmlElement, val active: Source[Boolean]) extends Component
private[stack] case class BannerBuilder(
    private val child: Source[HtmlElement],
    private val variant: Source[Banner.Variant],
    private val activeIn: Source[Boolean],
    private val activeOut: Sink[Boolean]
) extends Builder[Banner] {

  def build() = {
    val state = Var(false)

    val effectiveState = state.signal.changes
      .mergeWith(
        activeIn.toObservable.toWeakSignal
          .map(_.getOrElse(false))
          .changes
      )
      .toWeakSignal
      .map(_.getOrElse(false))
    val root = div(
      cls <-- variant.toObservable.map { variant =>
        val base = " s-banner"
        import Banner.Variant
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
      position := "static", // override stack css
      hidden <-- effectiveState.map(b => !b),
      div(
        cls := "d-flex flex__center s-banner__container",
        div(cls := "g8", L.child <-- child),
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
            onClick.mapToUnit.map(_ => false) --> state.writer,
            effectiveState --> activeOut
          )
        )
      )
    )
    new Banner(root, effectiveState)
  }

}

object Banner extends Companion[Banner, BannerBuilder] {

  val assignChild: In[K.child, HtmlElement] =
    mkIn((b, v) => b.copy(child = v))
  val assignV: In[K.variant, Variant] =
    mkIn((b, v) => b.copy(variant = v))
  val assignIn: In[K.active, Boolean] =
    mkIn((b, v) => b.copy(activeIn = v))
  val assignOut: Out[K.active, Boolean] =
    mkOut((b, v) => b.copy(activeOut = v))

  protected object keys {
    val child = new InSyntax[K.child, BannerBuilder, HtmlElement](assignChild)
    val variant = new InSyntax[K.variant, BannerBuilder, Variant](assignV)
    val active = new InOutSyntax[K.active, BannerBuilder, Boolean](assignIn, assignOut)
  }
  type X = keys.type
  val x = keys

  sealed trait Variant
  private[stack] object Variant {
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

  val empty = BannerBuilder(
    child = Signal.fromValue(div()),
    variant = Signal.fromValue(Info),
    activeIn = Signal.fromValue(false),
    activeOut = Observer.empty[Boolean]
  )

}
