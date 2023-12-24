package lui.stack
import com.raquo.laminar.api.L._
import com.raquo.laminar.api.L
import lui.util._
import lui._
import stack.{KeyTypes => K}

private[stack] case class ButtonBuilder(
    label: Source[String],
    value: Sink[Unit],
    size: Source[Button.Size],
    variant: Source[Button.Variant],
    selected: Source[Boolean],
    loading: Source[Boolean],
    dropdown: Source[Boolean],
    disabled: Source[Boolean]
) extends Builder[Button] {
  def build() = {
    val b = this
    import Button._
    val btnSizeMod =
      b.size.toObservable.toWeakSignal.map(_.getOrElse(Medium)).map {
        _ match {
          case Size.Small  => "s-btn__xs"
          case Size.Medium => ""
          case Size.Large  => "s-btn__md"
        }
      }
    val variantMod =
      b.variant.toObservable.toWeakSignal
        .map(_.getOrElse(Variant.SecondaryClear))
        .map {
          _ match {
            case Variant.Primary           => "s-btn__primary"
            case Variant.SecondaryClear    => ""
            case Variant.SecondaryOutlined => "s-btn__outlined"
            case Variant.SecondaryFilled   => "s-btn__filled"
            case Variant.Muted             => "s-btn__muted"
            case Variant.Danger            => "s-btn__danger"
          }
        }

    val disabled =
      b.disabled.toObservable.toWeakSignal.map(_.getOrElse(false))
    val selected =
      b.selected.toObservable.toWeakSignal.map(_.getOrElse(false)).map {
        _ match {
          case true => """is-selected"""
          case _    => ""
        }
      }
    val dropdown =
      b.dropdown.toObservable.toWeakSignal.map(_.getOrElse(false)).map {
        _ match {
          case true => """s-btn__dropdown"""
          case _    => ""
        }
      }
    val loading =
      b.loading.toObservable.toWeakSignal.map(_.getOrElse(false)).map {
        _ match {
          case true => """is-loading"""
          case _    => ""
        }
      }
    val combinedMods = Signal
      .combineSeq(
        List(
          btnSizeMod,
          selected,
          dropdown,
          loading,
          variantMod,
          btnSizeMod
        )
      )
      .map { list =>
        "s-btn " + list.mkString(" ", " ", " ")
      }

    val root = button(
      typ := "button",
      cls <-- combinedMods,
      aria.disabled <-- disabled,
      span(
        L.child.text <-- b.label
      ),
      onClick.mapToUnit --> b.value
    )
    new Button(root, root.events(onClick).mapToUnit)
  }
}

class Button private[stack] (
    val root: HtmlElement,
    val click: EventStream[Unit]
) extends Component

object Button extends Companion[Button, ButtonBuilder] {
  private val assignLabel: In[K.label, String] =
    mkIn((b, v) => b.copy(label = v))

  private val assignDisabled: In[K.disabled, Boolean] =
    mkIn((b, v) => b.copy(disabled = v))
  private val assignLoading: In[K.loading, Boolean] =
    mkIn((b, v) => b.copy(loading = v))
  private val assignDropdown: In[K.dropdown, Boolean] =
    mkIn((b, v) => b.copy(dropdown = v))
  private val assignSelected: In[K.selected, Boolean] =
    mkIn((b, v) => b.copy(selected = v))
  private val assignVariant: In[K.variant, Button.Variant] =
    mkIn((b, v) => b.copy(variant = v))
  private val assignSize: In[K.size, Button.Size] =
    mkIn((b, v) => b.copy(size = v))

  private val assignOut: Out[K.value, Unit] =
    mkOut((b, v) => b.copy(value = v))

  protected object keys {
    val dropdown =
      new InSyntax[K.dropdown, ButtonBuilder, Boolean](assignDropdown)
    val loading = new InSyntax[K.loading, ButtonBuilder, Boolean](assignLoading)
    val disabled =
      new InSyntax[K.disabled, ButtonBuilder, Boolean](assignDisabled)
    val label = new InSyntax[K.label, ButtonBuilder, String](assignLabel)
    val selected =
      new InSyntax[K.selected, ButtonBuilder, Boolean](assignSelected)
    val variant =
      new InSyntax[K.variant, ButtonBuilder, Button.Variant](assignVariant)
    val size = new InSyntax[K.size, ButtonBuilder, Button.Size](assignSize)
    val value = new OutSyntax[K.value, ButtonBuilder, Unit](assignOut)
  }
  type X = keys.type
  val x = keys

  def empty = ButtonBuilder(
    Signal.fromValue(""),
    Observer.empty[Unit],
    Signal.fromValue(Button.Medium),
    Signal.fromValue(Button.SecondaryClear),
    Signal.fromValue(false),
    Signal.fromValue(false),
    Signal.fromValue(false),
    Signal.fromValue(false)
  )

  sealed trait Variant
  private[stack] object Variant {
    case object Primary extends Variant
    case object SecondaryClear extends Variant
    case object SecondaryOutlined extends Variant
    case object SecondaryFilled extends Variant
    case object Muted extends Variant
    case object Danger extends Variant
  }
  val Primary: Variant = Variant.Primary
  val SecondaryClear: Variant = Variant.SecondaryClear
  val SecondaryOutlined: Variant = Variant.SecondaryOutlined
  val SecondaryFilled: Variant = Variant.SecondaryFilled
  val Muted: Variant = Variant.Muted
  val Danger: Variant = Variant.Danger

  sealed trait Size
  private[stack] object Size {
    case object Small extends Size
    case object Medium extends Size
    case object Large extends Size
  }
  val Small: Size = Size.Small
  val Medium: Size = Size.Medium
  val Large: Size = Size.Large

}
