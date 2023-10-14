package lui.stack
import com.raquo.laminar.api.L._
import com.raquo.laminar.api.L
import lui.util._
import lui.{_}
import stack.{KeyTypes => K}

private[stack] case class TextAreaBuilder(
    label: Source[String],
    description: Source[String],
    placeholder: Source[String],
    validationMessage: Source[String],
    value: Sink[String],
    size: Source[TextArea.Size],
    validation: Source[Option[TextArea.Validation]],
    disabled: Source[Boolean]
) extends Builder[TextArea] {
  def build(): TextArea = {
    val b = this
    import TextArea.Validation
    import TextArea.Size
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

    val i = textArea(
      cls <-- b.size.toObservable.map {
        _ match {
          case Size.Small      => "flex--item s-textarea s-textarea__sm"
          case Size.Medium     => "flex--item s-textarea"
          case Size.Large      => "flex--item s-textarea s-textarea__lg"
          case Size.ExtraLarge => "flex--item s-textarea s-textarea__xl"
        }
      },
      L.placeholder <-- b.placeholder,
      onInput.mapToValue --> b.value
    )

    val root = div(
      cls <-- containerStyle,
      L.label(
        cls := "flex--item d-block s-label",
        L.child.text <-- b.label,
        p(cls := "s-description mt2", L.child.text <-- b.description)
      ),
      div(
        cls := "flex--item d-flex ps-relative",
        i
      ),
      p(
        cls := "flex--item s-input-message mb0",
        L.child.text <-- b.validationMessage
      )
    )
    TextArea(
      root,
      i.events(onInput)
        .map(_ => i.ref.value)
        .toObservable
        .toWeakSignal
        .map(_.getOrElse(i.ref.value))
    )
  }
}

case class TextArea(root: HtmlElement, value: Signal[String]) extends Comp

object TextArea extends Companion[TextArea, TextAreaBuilder] {
  object keys
      extends LabelKey
      with DisabledKey
      with PlaceholderKey
      with MessageKey
      with DescriptionKey
      with VariantKey
      with SizeKey
      with ValueKey
  val x = keys
  type X = keys.type

  def empty = TextAreaBuilder(
    Signal.fromValue(""),
    Signal.fromValue(""),
    Signal.fromValue(""),
    Signal.fromValue(""),
    Observer.empty[String],
    Signal.fromValue(TextArea.Medium),
    Signal.fromValue(None),
    Signal.fromValue(false)
  )

  implicit val assignLabel: In[K.label, String] =
    mk((b, v) => b.copy(label = v))
  implicit val assignDescription: In[K.description, String] =
    mk((b, v) => b.copy(description = v))
  implicit val assignPlaceholder: In[K.placeholder, String] =
    mk((b, v) => b.copy(placeholder = v))
  implicit val assignMessage: In[K.message, String] =
    mk((b, v) => b.copy(validationMessage = v))

  implicit val assignDisabled: In[K.disabled, Boolean] =
    mk((b, v) => b.copy(disabled = v))

  implicit val assignVariant: In[K.variant, Option[TextArea.Validation]] =
    mk((b, v) => b.copy(validation = v))
  implicit val assignSize: In[K.size, TextArea.Size] =
    mk((b, v) => b.copy(size = v))

  implicit val assignOut: Out[K.value, String] =
    mk((b, v) => b.copy(value = v))

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
