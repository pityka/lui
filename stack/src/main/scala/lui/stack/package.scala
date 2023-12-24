package lui
import com.raquo.laminar.api.L.Signal

package object stack {

  def MenuItem[T](
      item: T
  )(mods: NavigationItem.keys[T] => util.Mod[NavigationItemBuilder[T]]*) =
    util
      .build(NavigationItem.empty(item))(
        mods.map(_(NavigationItem[T](item).keys)): _*
      )
      .copy(
        theme = Signal.fromValue(NavigationStyle.Menu)
      )
      .build()

  def MenuGroup[T](
      mods: NavigationGroup.keysObject[T] => util.Mod[
        NavigationGroupBuilder[T]
      ]*
  ) =
    util
      .build(NavigationGroupBuilder.empty[T])(
        mods.map(_(NavigationGroup[T].keys)): _*
      )
      .copy(
        theme = Signal.fromValue(NavigationStyle.Menu)
      )
      .build()

}
