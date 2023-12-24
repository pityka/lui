package lui.stack
import com.raquo.laminar.api.L._
import com.raquo.laminar.api.L
import lui.util._
import lui.{_}
private[stack] case class SelectBuilder(
    label: Source[String],
    description: Source[String],
    validationMessage: Source[String],
    options: Source[Seq[String]],
    value: Sink[Int],
    valueIn: Source[Int],
    size: Source[Select.Size],
    validation: Source[Option[Select.Validation]],
    disabled: Source[Boolean]
) extends Builder[Select] {
  def build() = {
    val b = this
    import Select.Validation
    import Select.Size
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
    new Select(root, i.events(onInput).map(_ => i.ref.value.toInt))
  }

}

class Select private[stack] (val root: HtmlElement, val value: EventStream[Int])
    extends Comp

object Select extends Companion[Select, SelectBuilder] {
  protected object keys
      extends LabelKey
      with DescriptionKey
      with MessageKey
      with DisabledKey
      with VariantKey
      with SizeKey
      with OptionsKey
      with ValueInOutKey {
    protected type SizeValue = Size
    protected type VariantValue = Option[Validation]
    protected type OptionsValue = Seq[String]
    protected type Builder = SelectBuilder

    protected val labelKey =
      mkIn((b, v) => b.copy(label = v))
    protected val descriptionKey =
      mkIn((b, v) => b.copy(description = v))
    protected val messageKey =
      mkIn((b, v) => b.copy(validationMessage = v))

    protected val disabledKey =
      mkIn((b, v) => b.copy(disabled = v))

    protected val variantKey =
      mkIn((b, v) => b.copy(validation = v))
    protected val optionsKey =
      mkIn((b, v) => b.copy(options = v))
    protected val sizeKey =
      mkIn((b, v) => b.copy(size = v))

    protected type ValueType = Int
    protected val valueKeyOut =
      mkOut((b, v) => b.copy(value = v))
    protected val valueKeyIn =
      mkIn((b, v) => b.copy(valueIn = v))
  }
  type X = keys.type
  val x = keys

  def empty = SelectBuilder(
    label = Signal.fromValue(""),
    description = Signal.fromValue(""),
    validationMessage = Signal.fromValue(""),
    options = Signal.fromValue(Nil),
    value = Observer.empty[Int],
    valueIn = Signal.fromValue(0),
    size = Signal.fromValue(Select.Medium),
    validation = Signal.fromValue(None),
    disabled = Signal.fromValue(false)
  )

  sealed trait Validation
  private[stack] object Validation {
    case object Warning extends Validation
    case object Error extends Validation
    case object Success extends Validation
  }
  val Warning: Validation = Validation.Warning
  val Error: Validation = Validation.Error
  val Success: Validation = Validation.Success

  sealed trait Size
  private[stack] object Size {
    case object Small extends Size
    case object Medium extends Size
    case object Large extends Size
    case object ExtraLarge extends Size
  }
  val Small: Size = Size.Small
  val Medium: Size = Size.Medium
  val Large: Size = Size.Large
  val ExtraLarge: Size = Size.ExtraLarge

}
