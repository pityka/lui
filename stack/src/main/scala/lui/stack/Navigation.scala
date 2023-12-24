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

case class NavigationItemBuilder[T](
    label: Source[String],
    item: T,
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
              if (activeKey.isDefined && activeKey.get == b.item) {
                "s-navigation--item is-selected "
              } else "s-navigation--item "
            case Menu => "s-block-link"
          }

        },
      L.child.text <-- b.label
    )

    val root = i
    new NavigationItem[T](
      root = root,
      clicks = i.events(onClick).mapTo(b.item),
      active = bus.writer
    )
  }
}

class NavigationItem[T] private[stack] (
    val root: HtmlElement,
    val clicks: EventStream[T],
    val active: Sink[T]
) extends Comp

object NavigationItem {
  class keys[T] extends LabelKey with ThemeKey {
    protected type Builder = NavigationItemBuilder[T]
    protected type ThemeValue = NavigationStyle

    protected val themeKey =
      mkIn((b, v) => b.copy(theme = v))
    protected val labelKey =
      mkIn((b, v) => b.copy(label = v))

  }

  class Helper[T](item: T, val keys: keys[T]) {
    def apply(
        mods: (keys[T] => util.Mod[NavigationItemBuilder[T]])*
    ) = {
      util
        .build(NavigationItem.empty(item))(mods.map(_(keys)): _*)
        .build()
    }
  }
  def apply[T](item: T) = {
    val keys = new keys[T]

    new Helper(item, keys)
  }

  def empty[T](t: T) = NavigationItemBuilder(
    Signal.fromValue(""),
    t,
    Signal.fromValue(NavigationStyle.NavigationHorizontal)
  )

}

case class NavigationGroupBuilder[T](
    children: Source[Seq[NavigationItem[T]]],
    value: Sink[T],
    valueIn: Source[T],
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
class NavigationGroup[T] private[stack] (
    val navigationElement: HtmlElement,
    val activeItem: EventStream[T]
) extends Comp {
  protected def root = navigationElement
}

object NavigationGroup {
  final class keysObject[T]
      extends ThemeKey
      with ChildKey
      with ChildrenKey
      with CheckedInOutKey {
    protected type Builder = NavigationGroupBuilder[T]
    protected type ChildValue = NavigationItem[T]
    protected type ThemeValue = NavigationStyle

    protected type CheckedValue = T

    protected val checkedKeyIn = mkIn((b, v) => b.copy(valueIn = v))
    protected val checkedKeyOut = mkOut((b, v) => b.copy(value = v))

    protected val themeKey =
      mkIn((b, v) => b.copy(theme = v))
    protected val childKey =
      mkIn((b, v) =>
        b.copy(children =
          b.children.toObservable.toWeakSignal
            .map(_.getOrElse(Nil))
            .combineWith(v.toObservable.toWeakSignal)
            .map { case (a, b) => a ++ b.toList }
        )
      )
    protected val childrenKey = mkIn((b, v) =>
      b.copy(children =
        b.children.toObservable.toWeakSignal
          .map(_.getOrElse(Nil))
          .combineWith(v.toObservable.toWeakSignal)
          .map { case (a, b) => a ++ b.toList.flatten }
      )
    )

  }
  class Helper[T](val keys: keysObject[T]) {
    def apply(
        mods: (keysObject[T] => util.Mod[NavigationGroupBuilder[T]])*
    ) = {
      util
        .build(NavigationGroupBuilder.empty[T])(mods.map(_(keys)): _*)
        .build()
    }
  }
  def apply[T] = {
    val keys = new keysObject[T]

    new Helper(keys)
  }
}

object NavigationGroupBuilder {

  def empty[T]: NavigationGroupBuilder[T] = NavigationGroupBuilder[T](
    children = Signal.fromValue(Nil),
    value = Observer.empty[T],
    valueIn = EventStream.empty,
    theme = Signal.fromValue(NavigationStyle.NavigationHorizontal)
  )

}
