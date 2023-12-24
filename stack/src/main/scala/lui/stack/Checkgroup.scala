package lui.stack
import com.raquo.laminar.api.L._
import com.raquo.laminar.api.L
import lui.util._
import lui._

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

    val checkedState = Var(false)

    val i = input(
      inChecked --> checkedState.writer,
      checkedState --> checked,
      L.checked <-- checkedState,
      onInput.mapToChecked --> checkedState.writer,
      typ := "checkbox",
      cls := "s-checkbox",
      L.disabled <-- b.disabled,
      idAttr := rand,
      L.value <-- b.inValue,
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
    new Checkbox(
      root,
      i.events(onInput)
        .map(_ => (i.ref.value, i.ref.checked))
        .toWeakSignal
        .map(
          _.getOrElse(
            (i.ref.value, i.ref.checked)
          )
        ),
        b.inValue,
        checkedState.writer
    )
  }
}

class Checkbox private[stack] (
    val root: HtmlElement,
    val checked: Signal[(String, Boolean)],
    val value: Source[String],
    val inChecked: Sink[Boolean]
) extends Component

object Checkbox extends Companion[Checkbox, CheckboxBuilder] {

  object keys
      extends LabelKey
      with DescriptionKey
      with MessageKey
      with DisabledKey
      with InValueKey
      with CheckedInOutKey
      with VariantKey {
    protected type VariantValue = Option[Validation]
    protected type Builder = CheckboxBuilder

    protected type CheckedValue = Boolean

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
    protected val checkedKeyIn =
      mkIn((b, v) => b.copy(inChecked = v))
    protected val inValueKey =
      mkIn((b, v) => b.copy(inValue = v))

    protected val checkedKeyOut =
      mkOut((b, v) => b.copy(checked = v))
  }

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

  // implicit val assignOut: Out[K.checked, Boolean] =

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
    checked: Sink[Seq[String]],
    checkedIn: Source[Seq[String]],
) extends Builder[CheckGroup] {
  def build() = {
    val b = this
    val containerStyle = b.horizontal.toObservable.map { h =>
      if (h) "s-check-group s-check-group__horizontal"
      else "s-check-group"
    }
    
    val unifiedSource: Signal[Seq[String]] = b.children.toObservable.toWeakSignal.flatMap {
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

    val xs: Signal[Observer[Seq[String]]] = 
      b.children.toObservable.toWeakSignal.map{
      case None => Observer.empty[Seq[String]]
      case Some(chil) =>
          Observer[Seq[String]]{ values =>
              chil.foreach{ checkbox =>
                checkbox.value.-->(checkboxValue => if (values.contains(checkboxValue)) {
                  checkbox.inChecked.toObserver.onNext(true)
                })  
              }
            } 
    }


    val root = fieldSet(
      xs.-->(observer => checkedIn.-->(values => observer.onNext(values))),
      cls <-- containerStyle,
      legend(cls := "s-label", L.child.text <-- b.label),
      L.children <-- b.children.toObservable.map(_.map(_.root)),
      unifiedSource --> b.checked
    )

    new CheckGroup(root, unifiedSource)

  }
}
class CheckGroup private[stack] (
    val root: HtmlElement,
    val checked: Signal[Seq[String]]
) extends Component
object CheckGroup extends Companion[CheckGroup, CheckGroupBuilder] {
  protected object keys
      extends LabelKey
      with HorizontalKey
      with ChildKey
      with ChildrenKey
      with CheckedInOutKey {
    protected type Builder = CheckGroupBuilder
    protected type ChildValue = Checkbox

    protected type CheckedValue = Seq[String]
    protected val checkedKeyOut =
      mkOut((b, v) => b.copy(checked = v))
    protected val checkedKeyIn =
      mkIn((b, v) => b.copy(checkedIn = v))
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

  def empty = CheckGroupBuilder(
    Signal.fromValue(""),
    Signal.fromValue(false),
    Signal.fromValue(Nil),
    Observer.empty[Seq[String]],
    Signal.fromValue(Nil)
  )

}
