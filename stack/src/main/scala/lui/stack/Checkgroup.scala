package lui.stack
import com.raquo.laminar.api.L._
import com.raquo.laminar.api.L
import lui.util._
import lui._
import stack.{KeyTypes => K}

private[stack] case class CheckboxBuilder(
    label: Source[String],
    description: Source[String],
    validationMessage: Source[String],
    inValue: Source[String],
    inChecked: Source[Boolean],
    checked: Sink[Boolean],
    validation: Source[Option[Checkbox.Validation]],
    disabled: Source[Boolean]
) extends Builder[Checkbox] {
  def build() = {
    val b = this
    import Checkbox.Validation
    val containerStyle = Signal
      .combine(
        b.disabled.toObservable.toWeakSignal.map(_.getOrElse(false)),
        b.validation.toObservable.toWeakSignal.map(_.flatten)
      )
      .map { case (readonly, validation) =>
        val base = "s-check-control "
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

    val rand = "radio-id-" + scala.util.Random.nextLong().toString

    val i = input(
      typ := "checkbox",
      cls := "s-checkbox",
      onInput.mapToChecked --> b.checked,
      L.disabled <-- b.disabled,
      idAttr := rand,
      L.value <-- b.inValue,
      L.checked <-- b.inChecked
    )

    val root = div(
      cls <-- containerStyle,
      i,
      L.label(
        forId := rand,
        cls := "flex--item d-block s-label",
        L.child.text <-- b.label,
        p(cls := "s-description mt2", L.child.text <-- b.description),
        p(
          cls := "flex--item s-input-message mb0",
          L.child.text <-- b.validationMessage
        )
      )
    )
    Checkbox(
      root,
      i.events(onInput)
        .map(_ => (i.ref.value, i.ref.checked))
        .toWeakSignal
        .map(
          _.getOrElse(
            (i.ref.value, i.ref.checked)
          )
        )
    )
  }
}

case class Checkbox(root: HtmlElement, checked: Signal[(String, Boolean)])
    extends Component

object Checkbox extends Companion[Checkbox, CheckboxBuilder] {

  object keys
      extends LabelKey
      with DescriptionKey
      with MessageKey
      with DisabledKey
      with InValueKey
      with InCheckedKey
      with VariantKey
      with CheckedKey

  type X = keys.type
  val x = keys

  def empty = CheckboxBuilder(
    Signal.fromValue(""),
    Signal.fromValue(""),
    Signal.fromValue(""),
    Signal.fromValue(""),
    Signal.fromValue(false),
    Observer.empty[Boolean],
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
  implicit val assignValue: In[K.inValue, String] =
    mk((b, v) => b.copy(inValue = v))
  implicit val assignChecked: In[K.inChecked, Boolean] =
    mk((b, v) => b.copy(inChecked = v))

  implicit val assignVariant: In[K.variant, Option[Checkbox.Validation]] =
    mk((b, v) => b.copy(validation = v))

  implicit val assignOut: Out[K.checked, Boolean] =
    mk((b, v) => b.copy(checked = v))

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
private[stack] case class CheckGroupBuilder(
    label: Source[String],
    horizontal: Source[Boolean],
    children: Source[Seq[Checkbox]],
    checked: Sink[Seq[String]]
) extends Builder[CheckGroup] {
  def build() = {
    val b = this
    val containerStyle = b.horizontal.toObservable.map { h =>
      if (h) "s-check-group s-check-group__horizontal"
      else "s-check-group"
    }
    val unifiedSource = b.children.toObservable.toWeakSignal.flatMap {
      case None => Signal.fromValue(Nil)
      case Some(chil) =>
        Signal
          .sequence(
            chil.map(
              _.checked.toObservable.toWeakSignal.map(_.filter(_._2).map(_._1))
            )
          )
          .map(_.flatten)
    }
    val root = fieldSet(
      cls <-- containerStyle,
      legend(cls := "s-label", L.child.text <-- b.label),
      L.children <-- b.children.toObservable.map(_.map(_.root)),
      unifiedSource --> b.checked
    )

    CheckGroup(root, unifiedSource)

  }
}
case class CheckGroup(root: HtmlElement, checked: Signal[Seq[String]])
    extends Component
object CheckGroup extends Companion[CheckGroup, CheckGroupBuilder] {
  object keys
      extends LabelKey
      with HorizontalKey
      with ChildKey
      with ChildrenKey
      with CheckedKey
  type X = keys.type
  val x = keys

  def empty = CheckGroupBuilder(
    Signal.fromValue(""),
    Signal.fromValue(false),
    Signal.fromValue(Nil),
    Observer.empty[Seq[String]]
  )

  implicit val assignLabel: In[K.label, String] =
    mk((b, v) => b.copy(label = v))
  implicit val assignHorizontal: In[K.horizontal, Boolean] =
    mk((b, v) => b.copy(horizontal = v))
  implicit val assignChild: In[K.child, Checkbox] =
    mk((b, v) =>
      b.copy(children =
        b.children.toObservable.toWeakSignal
          .map(_.getOrElse(Nil))
          .combineWith(v.toObservable.toWeakSignal)
          .map { case (a, b) => a ++ b.toList }
      )
    )
  implicit val assignChildren: In[K.children, Seq[Checkbox]] =
    mk((b, v) =>
      b.copy(children =
        b.children.toObservable.toWeakSignal
          .map(_.getOrElse(Nil))
          .combineWith(v.toObservable.toWeakSignal)
          .map { case (a, b) => a ++ b.toList.flatten }
      )
    )

  implicit val assignOut: Out[K.checked, Seq[String]] =
    mk((b, v) => b.copy(checked = v))

}
