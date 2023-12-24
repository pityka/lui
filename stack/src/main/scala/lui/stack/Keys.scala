package lui.stack
import KeyTypes._
import lui._

private[stack] trait KeyHelper {
  protected type Builder
  protected type In[K, V] = InKey[K, Builder, V]
  protected type Out[K, V] = OutKey[K, Builder, V]
  protected def in[K, V](k: In[K, V]) = new InSyntax[K, Builder, V](k)
  protected def out[K, V](k: Out[K, V]) = new OutSyntax[K, Builder, V](k)
  protected def inout[K, V](i: In[K,V], o: Out[K, V]) = new InOutSyntax[K, Builder, V](i,o)
}

private[stack] trait LabelKey extends KeyHelper {
  protected def labelKey: In[label, String]
  def label = in(labelKey)
}
private[stack] trait DisabledKey extends KeyHelper {
  protected def disabledKey: In[disabled, Boolean]
  def disabled = in(disabledKey)
}
private[stack] trait VariantKey extends KeyHelper {
  protected type VariantValue
  protected def variantKey: In[variant, VariantValue]
  def variant = in(variantKey)
}
private[stack] trait SelectedKey extends KeyHelper {
  protected def selectedKey: In[selected, Boolean]
  def selected = in(selectedKey)
}
private[stack] trait DropdownKey extends KeyHelper {
  protected def dropdownKey: In[dropdown, Boolean]
  def drowdown = in(dropdownKey)
}
private[stack] trait SizeKey extends KeyHelper {
  protected type SizeValue
  protected def sizeKey: In[size, SizeValue]
  def size = in(sizeKey)
}
private[stack] trait LoadingKey extends KeyHelper {
  protected def loadingKey: In[loading, Boolean]
  def loading = in(loadingKey)
}
private[stack] trait DescriptionKey extends KeyHelper {
  protected def descriptionKey: In[description, String]
  def description = in(descriptionKey)
}
private[stack] trait PlaceholderKey extends KeyHelper {
  protected def placeholderKey: In[placeholder, String]
  def placeholder = in(placeholderKey)

}
private[stack] trait MessageKey extends KeyHelper {
  protected def messageKey: In[message, String]
  def message = in(messageKey)
}
private[stack] trait OptionsKey extends KeyHelper {
  protected type OptionsValue
  protected def optionsKey: In[options, OptionsValue]
  def options = in(optionsKey)
}
private[stack] trait HorizontalKey extends KeyHelper {
  protected def horizontalKey: In[horizontal, Boolean]
  def horizontal = in(horizontalKey)
}
private[stack] trait ChildKey extends KeyHelper {
  protected type ChildValue
  protected def childKey: In[child, ChildValue]
  def child = in(childKey)
}
private[stack] trait ChildrenKey extends KeyHelper {
  protected type ChildValue
  protected def childrenKey: In[children, Seq[ChildValue]]
  def children = in(childrenKey)
}
private[stack] trait InValueKey extends KeyHelper {
  protected def inValueKey: In[inValue, String]

  /** Hidden value string which the form will return as value if selected */
  def valueIn = in(inValueKey)
}
private[stack] trait InCheckedKey extends KeyHelper {
  protected def inCheckedKey: In[inChecked, Boolean]
  def inChecked = in(inCheckedKey)
}
private[stack] trait ThemeKey extends KeyHelper {
  protected type ThemeValue
  protected def themeKey: In[theme, ThemeValue]
  def theme = in(themeKey)
}
private[stack] trait TitleKey extends KeyHelper {
  protected def titleKey: In[title, String]
  def title = in(titleKey)
}
private[stack] trait ActiveKey extends KeyHelper {
  protected def activeKey: In[active, Boolean]
  def active = in(activeKey)
}
private[stack] trait ActiveInOutKey extends KeyHelper {
  protected def activeKeyIn: In[active, Boolean]
  protected def activeKeyOut: Out[active, Boolean]
  def active = inout(activeKeyIn,activeKeyOut)
}
private[stack] trait WithCloseButtonKey extends KeyHelper {
  protected def withCloseButtonKey: In[withCloseButton, Boolean]
  def withCloseButton = in(withCloseButtonKey)

}
private[stack] trait ValueKey extends KeyHelper {
  protected type ValueType
  protected def valueKey: Out[value,ValueType]
  def value = out(valueKey)
  
}
private[stack] trait CheckedKey extends KeyHelper {
  protected type CheckedValue
  protected def checkedKey: Out[checked,CheckedValue]
  def checked = out(checkedKey)
}

private[stack] trait ValueInOutKey extends KeyHelper {
  protected type ValueType
  protected def valueKeyOut: Out[value,ValueType]
  protected def valueKeyIn: In[value,ValueType]
  def value = inout(valueKeyIn,valueKeyOut)
  
}
private[stack] trait CheckedInOutKey extends KeyHelper {
  protected type CheckedValue
  protected def checkedKeyOut: Out[checked,CheckedValue]
  protected def checkedKeyIn: In[checked,CheckedValue]
  def checked = inout(checkedKeyIn,checkedKeyOut)
}

