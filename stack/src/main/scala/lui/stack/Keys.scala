package lui.stack
import KeyTypes._
import lui.InSyntax
import lui.OutSyntax

private[stack] trait LabelKey {
  val label = new InSyntax[label]
}
private[stack] trait DisabledKey {
  val disabled = new InSyntax[disabled]
}
private[stack] trait VariantKey {
  val variant = new InSyntax[variant]
}
private[stack] trait SelectedKey {
  val selected = new InSyntax[selected]
}
private[stack] trait DropdownKey {
  val dropdown = new InSyntax[dropdown]
}
private[stack] trait SizeKey {
  val size = new InSyntax[size]
}
private[stack] trait LoadingKey {
  val loading = new InSyntax[loading]
}
private[stack] trait DescriptionKey {
  val description = new InSyntax[description]
}
private[stack] trait PlaceholderKey {
  val placeholder = new InSyntax[placeholder]
}
private[stack] trait MessageKey {
  val message = new InSyntax[message]
}
private[stack] trait OptionsKey {
  val options = new InSyntax[options]
}
private[stack] trait HorizontalKey {
  val horizontal = new InSyntax[horizontal]
}
private[stack] trait ChildKey {
  val child = new InSyntax[child]
}
private[stack] trait ChildrenKey {
  val children = new InSyntax[children]
}
private[stack] trait InValueKey {
  val inValue = new InSyntax[inValue]
}
private[stack] trait InCheckedKey {
  val inChecked = new InSyntax[inChecked]
}
private[stack] trait NameKey {
  val name = new InSyntax[name]
}
private[stack] trait ThemeKey {
  val theme = new InSyntax[theme]
}
private[stack] trait TitleKey {
  val title = new InSyntax[title]
}
private[stack] trait ActiveKey {
  val active = new InSyntax[active]
}
private[stack] trait WithCloseButtonKey {
  val withCloseButton = new InSyntax[withCloseButton]
}
private[stack] trait ValueKey {
  val value = new OutSyntax[value]
}
private[stack] trait CheckedKey {
  val checked = new OutSyntax[checked]
}

private[stack] trait KeyValues
    extends CheckedKey
    with ValueKey
    with WithCloseButtonKey
    with ActiveKey
    with TitleKey
    with ThemeKey
    with NameKey
    with InCheckedKey
    with InValueKey
    with ChildrenKey
    with ChildKey
    with HorizontalKey
    with OptionsKey
    with MessageKey
    with PlaceholderKey
    with DescriptionKey
    with LoadingKey
    with SizeKey
    with DropdownKey
    with SelectedKey
    with VariantKey
    with DisabledKey
    with LabelKey
