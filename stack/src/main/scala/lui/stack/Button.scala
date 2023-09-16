package lui.stack
import com.raquo.laminar.api.L._
import com.raquo.laminar.api.L
import lui.util._
import lui.{util, _}
import stack.{KeyTypes => K}

object Button {
  def apply(mods: util.Mod[Button.Param]*) =
    Button.fromParam(
      util.build(Button.Param.empty)(mods: _*)
    )
  case class Param(
      label: Source[String],
      value: Sink[Unit],
      size: Source[Size],
      variant: Source[Variant],
      selected: Source[Boolean],
      loading: Source[Boolean],
      dropdown: Source[Boolean],
      disabled: Source[Boolean]
  )
  object Param {
    def empty = Param(
      Signal.fromValue(""),
      Observer.empty[Unit],
      Signal.fromValue(Button.Medium),
      Signal.fromValue(Button.SecondaryClear),
      Signal.fromValue(false),
      Signal.fromValue(false),
      Signal.fromValue(false),
      Signal.fromValue(false)
    )
  }
  case class Component(root: HtmlElement, click: Source[Unit])
  object Component {
    import scala.language.implicitConversions
    implicit def conv(c: Component): HtmlElement = c.root
  }
  private type In[K, V] = Key[K, Param, Source[V]]
  private type Out[K, V] = Key[K, Param, Sink[V]]

  implicit val assignLabel: In[K.label, String] =
    mk((b, v) => b.copy(label = v))

  implicit val assignDisabled: In[K.disabled, Boolean] =
    mk((b, v) => b.copy(disabled = v))
  implicit val assignLoading: In[K.loading, Boolean] =
    mk((b, v) => b.copy(loading = v))
  implicit val assignDropdown: In[K.dropdown, Boolean] =
    mk((b, v) => b.copy(dropdown = v))
  implicit val assignSelected: In[K.selected, Boolean] =
    mk((b, v) => b.copy(selected = v))
  implicit val assignVariant: In[K.variant, Button.Variant] =
    mk((b, v) => b.copy(variant = v))
  implicit val assignSize: In[K.size, Button.Size] =
    mk((b, v) => b.copy(size = v))

  implicit val assignOut: Out[K.value, Unit] =
    mk((b, v) => b.copy(value = v))

  sealed trait Variant
  private[Button] object Variant {
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
  private[Button] object Size {
    case object Small extends Size
    case object Medium extends Size
    case object Large extends Size
  }
  val Small: Size = Size.Small
  val Medium: Size = Size.Medium
  val Large: Size = Size.Large

  def fromParam(b: Param) = {

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
      b.disabled.toObservable.toWeakSignal.map(_.getOrElse(false)).map {
        _ match {
          case true => """[aria-disabled="true"]"""
          case _    => ""
        }
      }
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
          disabled,
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
      span(
        L.child.text <-- b.label
      ),
      onClick.mapToUnit --> b.value
    )
    Component(root, root.events(onClick).mapToUnit)
  }

}
