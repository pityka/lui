package lui.stack
import com.raquo.laminar.api.L._
import com.raquo.laminar.api.L
import lui.util._
import lui.{util, _}
import stack.{KeyTypes => K}

object Select {
  def apply(mods: util.Mod[Select.Param]*) =
    Select.fromParam(
      util.build(Select.Param.empty)(mods: _*)
    )
  case class Param(
      label: Source[String],
      description: Source[String],
      validationMessage: Source[String],
      options: Source[Seq[String]],
      value: Sink[Int],
      size: Source[Size],
      validation: Source[Option[Validation]],
      disabled: Source[Boolean]
  )
  object Param {
    def empty = Param(
      Signal.fromValue(""),
      Signal.fromValue(""),
      Signal.fromValue(""),
      Signal.fromValue(Nil),
      Observer.empty[Int],
      Signal.fromValue(Select.Medium),
      Signal.fromValue(None),
      Signal.fromValue(false)
    )
  }
  case class Component(root: HtmlElement, value: Source[Int])
  object Component {
    import scala.language.implicitConversions
    implicit def conv(c: Component): HtmlElement = c.root
  }
  private type In[K, V] = Key[K, Param, Source[V]]
  private type Out[K, V] = Key[K, Param, Sink[V]]

  implicit val assignLabel: In[K.label, String] =
    mk((b, v) => b.copy(label = v))
  implicit val assignDescription: In[K.description, String] =
    mk((b, v) => b.copy(description = v))
  implicit val assignMessage: In[K.message, String] =
    mk((b, v) => b.copy(validationMessage = v))

  implicit val assignDisabled: In[K.disabled, Boolean] =
    mk((b, v) => b.copy(disabled = v))

  implicit val assignVariant: In[K.variant, Option[Select.Validation]] =
    mk((b, v) => b.copy(validation = v))
  implicit val assignSize: In[K.size, Select.Size] =
    mk((b, v) => b.copy(size = v))
  implicit val assignOptions: In[K.options, Seq[String]] =
    mk((b, v) => b.copy(options = v))

  implicit val assignOut: Out[K.value, Int] =
    mk((b, v) => b.copy(value = v))

  sealed trait Validation
  private[Select] object Validation {
    case object Warning extends Validation
    case object Error extends Validation
    case object Success extends Validation
  }
  val Warning: Validation = Validation.Warning
  val Error: Validation = Validation.Error
  val Success: Validation = Validation.Success

  sealed trait Size
  private[Select] object Size {
    case object Small extends Size
    case object Medium extends Size
    case object Large extends Size
    case object ExtraLarge extends Size
  }
  val Small: Size = Size.Small
  val Medium: Size = Size.Medium
  val Large: Size = Size.Large
  val ExtraLarge: Size = Size.ExtraLarge

  def fromParam(b: Param) = {

    val containerStyle = Signal
      .combine(
        b.disabled.toObservable.toWeakSignal.map(_.getOrElse(false)),
        b.validation.toObservable.toWeakSignal.map(_.flatten)
      )
      .map { case (readonly, validation) =>
        val base = "d-flex gy4 fd-column"
        val v1 =
          if (readonly) base + " is-readonly"
          else base
        validation match {
          case None                     => v1
          case Some(Validation.Success) => v1 + " has-success"
          case Some(Validation.Warning) => v1 + " has-warning"
          case Some(Validation.Error)   => v1 + " has-error"
        }
      }

    val i = select(
      L.children <-- b.options.toObservable.map { list =>
        list.zipWithIndex.map { case (text, idx) =>
          option(
            L.value := idx.toString,
            text
          )
        }
      },
      onInput.mapToValue.map(_.toInt) --> b.value
    )

    val root = div(
      cls <-- containerStyle,
      L.label(
        cls := "flex--item d-block s-label",
        L.child.text <-- b.label,
        p(cls := "s-description mt2", L.child.text <-- b.description)
      ),
      div(
        cls <-- b.size.toObservable.map {
          _ match {
            case Size.Small      => "flex--item s-select s-select__sm"
            case Size.Medium     => "flex--item s-select"
            case Size.Large      => "flex--item s-select s-select__lg"
            case Size.ExtraLarge => "flex--item s-select s-select__xl"
          }
        },
        i
      ),
      p(
        cls := "flex--item s-input-message mb0",
        L.child.text <-- b.validationMessage
      )
    )
    Component(root, i.events(onInput).map(_ => i.ref.value.toInt))
  }

}
