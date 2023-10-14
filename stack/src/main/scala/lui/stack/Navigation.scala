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

private[stack] case class NavigationItemBuilder[T](
    label: Source[String],
    key: T,
    theme: Source[NavigationStyle]
) extends Builder[NavigationItem[T]] {
  def build(): NavigationItem[T] = {
    val b = this
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
    NavigationItem[T](
      root = root,
      clicks = i.events(onClick).mapTo(b.key),
      active = bus.writer
    )
  }
}

case class NavigationItem[T](
    root: HtmlElement,
    clicks: EventStream[T],
    active: Sink[T]
) extends Comp

object NavigationItem {
  object keys extends LabelKey with ThemeKey
  def apply[T](key: T)(mods: keys.type => util.Mod[NavigationItemBuilder[T]]*) =
    util.build(empty(key))(mods.map(_(keys)): _*).build()

  def empty[T](t: T) = NavigationItemBuilder(
    Signal.fromValue(""),
    t,
    Signal.fromValue(NavigationStyle.NavigationHorizontal)
  )

  private type In[K, V, T] = Key[K, NavigationItemBuilder[T], Source[V]]

  implicit def assignLabel[T]: In[K.label, String, T] =
    mk((b, v) => b.copy(label = v))
  implicit def assignTheme[T]: In[K.theme, NavigationStyle, T] =
    mk((b, v) => b.copy(theme = v))

}

 case class NavigationGroupBuilder[T](
    children: Source[Seq[NavigationItem[T]]],
    value: Sink[T],
    theme: Source[NavigationStyle]
) extends Builder[NavigationGroup[T]] {
  def build() = {
    val b = this
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

    new NavigationGroup(root, lastClick)

  }
}
 class NavigationGroup[T](val root: HtmlElement,val value: EventStream[T])
    extends Comp

object NavigationGroup {
  object keysObject
      extends LabelKey
      with ThemeKey
      with ChildKey
      with ChildrenKey
      with CheckedKey
    val keys = keysObject
  def apply[T](mods: keysObject.type => util.Mod[NavigationGroupBuilder[T]]*) =
    util.build(NavigationGroupBuilder.empty[T])(mods.map(_(keys)): _*).build()
}

object NavigationGroupBuilder {
  

  def empty[T] : NavigationGroupBuilder[T] = NavigationGroupBuilder[T](
    children = Signal.fromValue(Nil),
    value = Observer.empty[T],
    theme = Signal.fromValue(NavigationStyle.NavigationHorizontal)
  )

  private type In[K, V, T] = Key[K, NavigationGroupBuilder[T], Source[V]]
  private type Out[K, V, T] = Key[K, NavigationGroupBuilder[T], Sink[V]]

  implicit def assignChild[T]: In[K.child, NavigationItem[T], T] =
    mk((b, v) =>
      b.copy(children =
        b.children.toObservable.toWeakSignal
          .map(_.getOrElse(Nil))
          .combineWith(v.toObservable.toWeakSignal)
          .map { case (a, b) => a ++ b.toList }
      )
    )
  implicit def assignChildren[T]: In[K.children, Seq[NavigationItem[T]], T] =
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

}
