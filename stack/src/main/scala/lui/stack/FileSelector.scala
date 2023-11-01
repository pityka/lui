package lui.stack
import com.raquo.laminar.api.L._
import com.raquo.laminar.api.L
import lui.util._
import lui.{_}
import stack.{KeyTypes => K}
import org.scalajs.dom

private[stack] case class FileSelectorBuilder(
    label: Source[String],
    description: Source[String],
    validationMessage: Source[String],
    value: Sink[List[dom.File]],
    validation: Source[Option[FileSelector.Validation]],
    disabled: Source[Boolean]
) extends Builder[FileSelector] {
  def build(): FileSelector = {
    val b = this
    import FileSelector.Validation
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

    val i = input(
      typ := "file",
      cls :="flex--item",
      padding := "5px",
      onInput.mapToFiles --> b.value
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
    FileSelector(
      root,
      i.events(onInput)
        .map(_ => i.ref.files.toList)
        .toObservable
        .toWeakSignal
        .map(_.getOrElse(i.ref.files.toList))
    )
  }
}
case class FileSelector(root: HtmlElement, value: Signal[List[dom.File]]) extends Comp
object FileSelector extends Companion[FileSelector, FileSelectorBuilder] {
  object keys
      extends LabelKey
      with DescriptionKey
      with MessageKey
      with DisabledKey
      with VariantKey
      with ValueKey
  type X = keys.type
  val x = keys
  def empty = FileSelectorBuilder(
    Signal.fromValue(""),
    Signal.fromValue(""),
    Signal.fromValue(""),
    Observer.empty[List[dom.File]],
    Signal.fromValue(None),
    Signal.fromValue(false)
  )

  implicit val assignLabel: In[K.label, String] =
    mk((b, v) => b.copy(label = v))
  implicit val assignDescription: In[K.description, String] =
    mk((b, v) => b.copy(description = v))
  implicit val assignMessage: In[K.message, String] =
    mk((b, v) => b.copy(validationMessage = v))

  implicit val assignDisabled: In[K.disabled, Boolean] =
    mk((b, v) => b.copy(disabled = v))

  implicit val assignVariant: In[K.variant, Option[FileSelector.Validation]] =
    mk((b, v) => b.copy(validation = v))

  implicit val assignOut: Out[K.value, List[dom.File]] =
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

  
}
