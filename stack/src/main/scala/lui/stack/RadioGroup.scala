package lui.stack
import com.raquo.laminar.api.L._
import com.raquo.laminar.api.L
import lui.util._
import lui.{util, _}
import stack.{KeyTypes => K}

object RadioOption {
  def apply(mods: util.Mod[RadioOption.Param]*) =
    RadioOption.fromParam(
      util.build(RadioOption.Param.empty)(mods: _*)
    )
  case class Param(
      label: Source[String],
      description: Source[String],
      validationMessage: Source[String],
      inValue: Source[String],
      name: Source[String],
      checked: Sink[Boolean],
      validation: Source[Option[Validation]],
      disabled: Source[Boolean]
  )
  object Param {
    def empty = Param(
      Signal.fromValue(""),
      Signal.fromValue(""),
      Signal.fromValue(""),
      Signal.fromValue(""),
      Signal.fromValue(""),
      Observer.empty[Boolean],
      Signal.fromValue(None),
      Signal.fromValue(false)
    )
  }
  case class Component(root: HtmlElement, checked: Signal[(String, Boolean)]) extends Comp
  
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
  implicit val assignValue: In[K.inValue, String] =
    mk((b, v) => b.copy(inValue = v))
  implicit val assignName: In[K.name, String] =
    mk((b, v) => b.copy(name = v))

  implicit val assignVariant: In[K.variant, Option[RadioOption.Validation]] =
    mk((b, v) => b.copy(validation = v))

  implicit val assignOut: Out[K.checked, Boolean] =
    mk((b, v) => b.copy(checked = v))

  sealed trait Validation
  private[RadioOption] object Validation {
    case object Warning extends Validation
    case object Error extends Validation
    case object Success extends Validation
  }
  val Warning: Validation = Validation.Warning
  val Error: Validation = Validation.Error
  val Success: Validation = Validation.Success

  def fromParam(b: Param) = {

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
    Component(
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

object RadioGroup {
  def apply(mods: util.Mod[RadioGroup.Param]*) =
    RadioGroup.fromParam(
      util.build(RadioGroup.Param.empty)(mods: _*)
    )
  case class Param(
      label: Source[String],
      horizontal: Source[Boolean],
      children: Source[Seq[RadioOption.Component]],
      checked: Sink[Option[String]]
  )
  object Param {
    def empty = Param(
      Signal.fromValue(""),
      Signal.fromValue(false),
      Signal.fromValue(Nil),
      Observer.empty[Option[String]]
    )
  }
  case class Component(root: HtmlElement, checked: Signal[Option[String]]) extends Comp
  
  private type In[K, V] = Key[K, Param, Source[V]]
  private type Out[K, V] = Key[K, Param, Sink[V]]

  implicit val assignLabel: In[K.label, String] =
    mk((b, v) => b.copy(label = v))
  implicit val assignHorizontal: In[K.horizontal, Boolean] =
    mk((b, v) => b.copy(horizontal = v))
  implicit val assignChild: In[K.child, RadioOption.Component] =
    mk((b, v) =>
      b.copy(children =
        b.children.toObservable.toWeakSignal
          .map(_.getOrElse(Nil))
          .combineWith(v.toObservable.toWeakSignal)
          .map { case (a, b) => a ++ b.toList }
      )
    )
  implicit val assignChildren: In[K.children, Seq[RadioOption.Component]] =
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

  def fromParam(b: Param) = {

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

    Component(root, unifiedSource)

  }

}
