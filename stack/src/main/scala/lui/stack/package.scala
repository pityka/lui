package lui
import com.raquo.laminar.api.L.Signal

package object stack extends KeyValues {

  def MenuItem[T](
      key: T
  )(mods: NavigationItem.keys.type => util.Mod[NavigationItemBuilder[T]]*) =
    util
      .build(NavigationItem.empty(key))(mods.map(_(NavigationItem.keys)): _*)
      .copy(
        theme = Signal.fromValue(NavigationStyle.Menu)
      )
      .build()

  def MenuGroup[T](
      mods: NavigationGroup.keys.type => util.Mod[NavigationGroupBuilder[T]]*
  ) =
    util
      .build(NavigationGroupBuilder.empty[T])(mods.map(_(NavigationGroup.keys)): _*)
      .copy(
        theme = Signal.fromValue(NavigationStyle.Menu)
      )
      .build()

}
