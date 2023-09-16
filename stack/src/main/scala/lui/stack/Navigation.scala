package lui.stack
import com.raquo.laminar.api.L._
import com.raquo.laminar.api.L
import lui.util._
import lui.{util, _}
import com.raquo.airstream.eventbus
import com.raquo.laminar.nodes.ReactiveElement
import lui.stack.NavigationStyleEnum.NavigationVertical
import lui.stack.NavigationStyleEnum.NavigationHorizontal
import lui.stack.NavigationStyleEnum.NavigationMuted
import lui.stack.NavigationStyleEnum.Menu
import stack.{KeyTypes => K}

sealed trait NavigationStyle
private[stack] object NavigationStyleEnum {
  case object NavigationVertical extends NavigationStyle
  case object NavigationHorizontal extends NavigationStyle
  case object NavigationMuted extends NavigationStyle
  case object Menu extends NavigationStyle
}
object NavigationStyle {

  val NavigationVertical: NavigationStyle =
    NavigationStyleEnum.NavigationVertical
  val NavigationHorizontal: NavigationStyle =
    NavigationStyleEnum.NavigationHorizontal
  val Menu: NavigationStyle = NavigationStyleEnum.Menu
  val NavigationMuted: NavigationStyle = NavigationStyleEnum.NavigationMuted
}

object NavigationItem {
  def apply[T](key: T)(mods: util.Mod[NavigationItem.Param[T]]*) =
    NavigationItem.fromParam(
      util.build(NavigationItem.Param.empty(key))(mods: _*)
    )
  case class Param[T](
      label: Source[String],
      key: T,
      theme: Source[NavigationStyle]
  )
  object Param {
    def empty[T](t: T) = Param(
      Signal.fromValue(""),
      t,
      Signal.fromValue(NavigationStyle.NavigationHorizontal)
    )
  }
  case class Component[T](root: HtmlElement, clicks: Source[T], active: Sink[T])
  object Component {
    import scala.language.implicitConversions
    implicit def conv(c: Component[_]): HtmlElement = c.root
  }
  private type In[K, V, T] = Key[K, Param[T], Source[V]]

  implicit def assignLabel[T]: In[K.label, String, T] =
    mk((b, v) => b.copy(label = v))
  implicit def assignTheme[T]: In[K.theme, NavigationStyle, T] =
    mk((b, v) => b.copy(theme = v))

  def fromParam[T](b: Param[T]) = {

    val bus = eventbus.EventBus[T]()
    val i = button(
      typ := "button",
      cls <-- bus.events
        .map(Some(_))
        .toSignal(None)
        .withCurrentValueOf(
          b.theme.toObservable.toWeakSignal
            .map(_.getOrElse(NavigationStyle.NavigationHorizontal))
        )
        .map { case (activeKey, theme) =>
          theme match {
            case NavigationHorizontal | NavigationVertical | NavigationMuted =>
              if (activeKey.isDefined && activeKey.get == b.key) {
                "s-navigation--item is-selected "
              } else "s-navigation--item "
            case Menu => "s-block-link"
          }

        },
      L.child.text <-- b.label
    )

    val root = i
    Component(
      root = root,
      clicks = i.events(onClick).mapTo(b.key),
      active = bus.writer
    )
  }

}

object NavigationGroup {
  def apply[T](mods: util.Mod[NavigationGroup.Param[T]]*) =
    NavigationGroup.fromParam(
      util.build(NavigationGroup.Param.empty[T])(mods: _*)
    )
  case class Param[T](
      children: Source[Seq[NavigationItem.Component[T]]],
      value: Sink[T],
      theme: Source[NavigationStyle]
  )
  object Param {
    def empty[T] = Param[T](
      children = Signal.fromValue(Nil),
      value = Observer.empty[T],
      theme = Signal.fromValue(NavigationStyle.NavigationHorizontal)
    )
  }
  case class Component[T](root: HtmlElement, value: Source[T])
  object Component {
    import scala.language.implicitConversions
    implicit def conv(c: Component[_]): HtmlElement = c.root
  }
  private type In[K, V, T] = Key[K, Param[T], Source[V]]
  private type Out[K, V, T] = Key[K, Param[T], Sink[V]]

  implicit def assignChild[T]: In[K.child, NavigationItem.Component[T], T] =
    mk((b, v) =>
      b.copy(children =
        b.children.toObservable.toWeakSignal
          .map(_.getOrElse(Nil))
          .combineWith(v.toObservable.toWeakSignal)
          .map { case (a, b) => a ++ b.toList }
      )
    )
  implicit def assignChildren[T]
      : In[K.children, Seq[NavigationItem.Component[T]], T] =
    mk((b, v) =>
      b.copy(children =
        b.children.toObservable.toWeakSignal
          .map(_.getOrElse(Nil))
          .combineWith(v.toObservable.toWeakSignal)
          .map { case (a, b) => a ++ b.toList.flatten }
      )
    )

  implicit def assignOut[T]: Out[K.checked, T, T] =
    mk((b, v) => b.copy(value = v))

  implicit def assignTheme[T]: In[K.theme, NavigationStyle, T] =
    mk((b, v) => b.copy(theme = v))

  def fromParam[T](b: Param[T]) = {

    val containerStyle = b.theme.toObservable.map { h =>
      h match {
        case NavigationVertical   => "s-navigation s-navigation__vertical"
        case NavigationHorizontal => "s-navigation"
        case NavigationMuted      => "s-navigation s-navigation__muted"
        case Menu                 => "s-menu"
      }

    }

    val lastClick = b.children.toObservable.flatMap { c =>
      EventStream.mergeSeq(
        c.map(_.clicks.toObservable.toWeakSignal.changes.collect {
          case Some(x) => x
        })
      )
    }.distinct

    val t: Signal[Observer[T]] = b.children.toObservable
      .map { children =>
        Observer.combine(children.map(_.active.toObserver): _*)
      }
      .toWeakSignal
      .map(_.getOrElse(Observer.empty[T]))
      .distinct

    val root = fieldSet(
      cls <-- containerStyle,
      L.children <-- b.children.toObservable.map(_.map(_.root)),
      lastClick --> b.value
    )

    ReactiveElement.bindFn(root, t) { observer =>
      (lastClick --> observer).bind(root)
    }

    Component(root, lastClick)

  }

}
