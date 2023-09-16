package lui
import com.raquo.laminar.api.L.Signal

package object stack extends KeyValues {

  def MenuItem[T](key: T)(mods: util.Mod[NavigationItem.Param[T]]*) =
    NavigationItem.fromParam(
      util
        .build(NavigationItem.Param.empty(key))(mods: _*)
        .copy(
          theme = Signal.fromValue(NavigationStyle.Menu)
        )
    )
  def MenuGroup[T](mods: util.Mod[NavigationGroup.Param[T]]*) =
    NavigationGroup.fromParam(
      util
        .build(NavigationGroup.Param.empty[T])(mods: _*)
        .copy(
          theme = Signal.fromValue(NavigationStyle.Menu)
        )
    )

}
