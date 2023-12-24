package lui.stack
import com.raquo.laminar.api.L._
import com.raquo.laminar.api.L
import lui.util._
import lui.{_}
import java.util.UUID

private[stack] case class RadioOptionBuilder(
    label: Source[String],
    description: Source[String],
    validationMessage: Source[String],
    inValue: Source[String],
    checked: Sink[Boolean],
    checkedIn: Source[Boolean],
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
      L.value <-- b.inValue,
      L.disabled <-- b.disabled,
      idAttr := rand
    )

    def changeName(n: String): Unit = i.amend(nameAttr := n)

    val root = {
      div(
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
    }
    RadioOption(
      root,
      i.events(onInput)
        .map(_ => (i.ref.value, i.ref.checked))
        .toWeakSignal
        .map(
          _.getOrElse(
            (i.ref.value, i.ref.checked)
          )
        ),
      changeName _
    )
  }
}
case class RadioOption(
    root: HtmlElement,
    checked: Signal[(String, Boolean)],
    changeName: String => Unit
) extends Comp

object RadioOption extends Companion[RadioOption, RadioOptionBuilder] {
  protected object keys
      extends LabelKey
      with DescriptionKey
      with MessageKey
      with DisabledKey
      with InValueKey
      with VariantKey
      with CheckedInOutKey {
    type VariantValue = Option[Validation]
    type InValueType = String
    protected type Builder = RadioOptionBuilder

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
    protected val inValueKey =
      mkIn((b, v) => b.copy(inValue = v))

    protected type CheckedValue = Boolean
    protected val checkedKeyOut =
      mkOut((b, v) => b.copy(checked = v))
    protected val checkedKeyIn =
      mkIn((b, v) => b.copy(checkedIn = v))
  }
  type X = keys.type
  val x = keys

  def empty = RadioOptionBuilder(
    Signal.fromValue(""),
    Signal.fromValue(""),
    Signal.fromValue(""),
    Signal.fromValue(""),
    Observer.empty[Boolean],
    Signal.fromValue(false),
    Signal.fromValue(None),
    Signal.fromValue(false)
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
    val name = UUID.randomUUID().toString()
    val root = fieldSet(
      cls <-- containerStyle,
      legend(cls := "s-label", L.child.text <-- b.label),
      L.children <-- b.children.toObservable.map(_.map { option =>
        option.changeName(name)
        option.root
      }),
      unifiedSource.changes --> b.checked
    )

    new RadioGroup(root, unifiedSource)

  }
}
class RadioGroup private[stack] (
    val root: HtmlElement,
    val checked: Signal[Option[String]]
) extends Comp
object RadioGroup extends Companion[RadioGroup, RadioGroupBuilder] {

  protected object keys
      extends LabelKey
      with HorizontalKey
      with ChildKey
      with ChildrenKey
      with CheckedKey {

    protected type Builder = RadioGroupBuilder
    protected type ChildValue = RadioOption

    protected type CheckedValue = Option[String]
    protected val checkedKey =
      mkOut((b, v) => b.copy(checked = v))

    protected val labelKey =
      mkIn((b, v) => b.copy(label = v))
    protected val horizontalKey =
      mkIn((b, v) => b.copy(horizontal = v))
    protected val childKey =
      mkIn((b, v) =>
        b.copy(children =
          b.children.toObservable.toWeakSignal
            .map(_.getOrElse(Nil))
            .combineWith(v.toObservable.toWeakSignal)
            .map { case (a, b) => a ++ b.toList }
        )
      )
    protected val childrenKey =
      mkIn((b, v) =>
        b.copy(children =
          b.children.toObservable.toWeakSignal
            .map(_.getOrElse(Nil))
            .combineWith(v.toObservable.toWeakSignal)
            .map { case (a, b) => a ++ b.toList.flatten }
        )
      )

  }
  type X = keys.type
  val x = keys

  def empty = RadioGroupBuilder(
    Signal.fromValue(""),
    Signal.fromValue(false),
    Signal.fromValue(Nil),
    Observer.empty[Option[String]]
  )

}
