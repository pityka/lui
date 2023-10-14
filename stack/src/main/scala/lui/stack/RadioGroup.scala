package lui.stack
import com.raquo.laminar.api.L._
import com.raquo.laminar.api.L
import lui.util._
import lui.{_}
import stack.{KeyTypes => K}

private[stack] case class RadioOptionBuilder(
    label: Source[String],
    description: Source[String],
    validationMessage: Source[String],
    inValue: Source[String],
    name: Source[String],
    checked: Sink[Boolean],
    validation: Source[Option[RadioOption.Validation]],
    disabled: Source[Boolean]
) extends Builder[RadioOption] {
  def build() = {
    val b = this
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
          case None                                 => v1
          case Some(RadioOption.Validation.Success) => v1 + " has-success"
          case Some(RadioOption.Validation.Warning) => v1 + " has-warning"
          case Some(RadioOption.Validation.Error)   => v1 + " has-error"
        }
      }

    val rand = "radio-id-" + scala.util.Random.nextLong().toString

    val i = input(
      typ := "radio",
      cls := "s-radio",
      onInput.mapToChecked --> b.checked,
      nameAttr <-- b.name,
      L.value <-- b.inValue,
      L.disabled <-- b.disabled,
      idAttr := rand
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
    RadioOption(
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
case class RadioOption(root: HtmlElement, checked: Signal[(String, Boolean)])
    extends Comp

object RadioOption extends Companion[RadioOption, RadioOptionBuilder] {
  object keys
      extends LabelKey
      with DescriptionKey
      with MessageKey
      with DisabledKey
      with InValueKey
      with NameKey
      with VariantKey
      with CheckedKey
  type X = keys.type
  val x = keys

  def empty = RadioOptionBuilder(
    Signal.fromValue(""),
    Signal.fromValue(""),
    Signal.fromValue(""),
    Signal.fromValue(""),
    Signal.fromValue(""),
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
  implicit val assignName: In[K.name, String] =
    mk((b, v) => b.copy(name = v))

  implicit val assignVariant: In[K.variant, Option[RadioOption.Validation]] =
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

private[stack] case class RadioGroupBuilder(
    label: Source[String],
    horizontal: Source[Boolean],
    children: Source[Seq[RadioOption]],
    checked: Sink[Option[String]]
) extends Builder[RadioGroup] {
  def build() = {
    val b = this
    val containerStyle = b.horizontal.toObservable.map { h =>
      if (h) "s-check-group s-check-group__horizontal"
      else "s-check-group"
    }
    val unifiedSource = b.children.toObservable.toWeakSignal.flatMap {
      case None => Signal.fromValue(None)
      case Some(chil) =>
        Signal
          .sequence(
            chil.map(
              _.checked.toObservable.toWeakSignal.map(_.filter(_._2).map(_._1))
            )
          )
          .map(_.flatten.headOption)
    }
    val root = fieldSet(
      cls <-- containerStyle,
      legend(cls := "s-label", L.child.text <-- b.label),
      L.children <-- b.children.toObservable.map(_.map(_.root)),
      unifiedSource.changes --> b.checked
    )

    RadioGroup(root, unifiedSource)

  }
}
case class RadioGroup(root: HtmlElement, checked: Signal[Option[String]])
    extends Comp
object RadioGroup extends Companion[RadioGroup, RadioGroupBuilder] {

  object keys
      extends LabelKey
      with HorizontalKey
      with ChildKey
      with ChildrenKey
      with CheckedKey
  type X = keys.type
  val x = keys

  def empty = RadioGroupBuilder(
    Signal.fromValue(""),
    Signal.fromValue(false),
    Signal.fromValue(Nil),
    Observer.empty[Option[String]]
  )

  implicit val assignLabel: In[K.label, String] =
    mk((b, v) => b.copy(label = v))
  implicit val assignHorizontal: In[K.horizontal, Boolean] =
    mk((b, v) => b.copy(horizontal = v))
  implicit val assignChild: In[K.child, RadioOption] =
    mk((b, v) =>
      b.copy(children =
        b.children.toObservable.toWeakSignal
          .map(_.getOrElse(Nil))
          .combineWith(v.toObservable.toWeakSignal)
          .map { case (a, b) => a ++ b.toList }
      )
    )
  implicit val assignChildren: In[K.children, Seq[RadioOption]] =
    mk((b, v) =>
      b.copy(children =
        b.children.toObservable.toWeakSignal
          .map(_.getOrElse(Nil))
          .combineWith(v.toObservable.toWeakSignal)
          .map { case (a, b) => a ++ b.toList.flatten }
      )
    )

  implicit val assignOut: Out[K.checked, Option[String]] =
    mk((b, v) => b.copy(checked = v))

}
