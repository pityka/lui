package lui.stack
import KeyTypes._
import lui.InSyntax
import lui.OutSyntax

private[stack] trait KeyValues {

  val label = new InSyntax[label]
  val disabled = new InSyntax[disabled]
  val variant = new InSyntax[variant]
  val selected = new InSyntax[selected]
  val dropdown = new InSyntax[dropdown]
  val size = new InSyntax[size]
  val loading = new InSyntax[loading]
  val description = new InSyntax[description]
  val placeholder = new InSyntax[placeholder]
  val message = new InSyntax[message]
  val options = new InSyntax[options]
  val horizontal = new InSyntax[horizontal]
  val child = new InSyntax[child]
  val children = new InSyntax[children]
  val inValue = new InSyntax[inValue]
  val name = new InSyntax[name]
  val theme = new InSyntax[theme]
  val title = new InSyntax[title]
  val active = new InSyntax[active]

  val value = new OutSyntax[value]
  val checked = new OutSyntax[checked]
}
