package lui.stack
import com.raquo.laminar.api.L._
import com.raquo.laminar.api.L
import lui.util._
import lui.{_}
import org.scalajs.dom

private[stack] case class FileSelectorBuilder(
    label: Source[String],
    description: Source[String],
    validationMessage: Source[String],
    value: Sink[List[dom.File]],
    valueIn: Source[List[dom.File]],
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
      cls := "flex--item",
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

    val fileSourceFromInput = i
      .events(onInput)
      .map(_ => i.ref.files.toList)
      .toObservable
      .toWeakSignal
      .map(_.getOrElse(i.ref.files.toList))

    new FileSelector(
      root,
      fileSourceFromInput.changes
        .mergeWith(valueIn.toObservable.toWeakSignal.changes.collect({
          case Some(x) => x
        }))
        .toSignal(Nil)
    )
  }
}
class FileSelector private[stack] (
    val root: HtmlElement,
    val value: Signal[List[dom.File]]
) extends Comp
object FileSelector extends Companion[FileSelector, FileSelectorBuilder] {
  protected object keys
      extends LabelKey
      with DescriptionKey
      with MessageKey
      with DisabledKey
      with VariantKey
      with ValueInOutKey {
    protected type VariantValue = Option[Validation]
    protected type Builder = FileSelectorBuilder

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

    protected type ValueType = List[dom.File]
    protected val valueKeyOut =
      mkOut((b, v) => b.copy(value = v))
    protected val valueKeyIn =
      mkIn((b, v) => b.copy(valueIn = v))
  }
  type X = keys.type
  val x = keys
  def empty = FileSelectorBuilder(
    label = Signal.fromValue(""),
    description = Signal.fromValue(""),
    validationMessage = Signal.fromValue(""),
    value = Observer.empty[List[dom.File]],
    valueIn = Signal.fromValue(Nil),
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

}
