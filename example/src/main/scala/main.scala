package example

import com.raquo.laminar.api.L._
import lui.{stack => st}
object Demo {
  val s = Signal.fromValue("s")
  val but = st.Button(
    st.label := "sdsdf",
    st.loading := false,
    st.value --> Observer[Unit](x => println(x))
  )
  def root() = {
    div(
      div(
        st
          .TextField(
            st.label := "Name",
            st.placeholder := "placeholder..",
            st.description := "description",
            st.message := "message",
            st.value --> Observer[String](println)
          )
      ),
      div(
        st.TextArea(
          st.label := "Name",
          st.placeholder := "placeholder..",
          st.description := "description",
          st.message := "message",
          st.value --> Observer[String](println)
        )
      ),
      div(
        st.Select(
          st.label := "Name",
          st.description := "description",
          st.message := "message",
          st.options := Seq("a", "b", "c"),
          st.value --> Observer[Int](println)
        )
      ),
      div(
        st.CheckGroup(
          st.label := "group name",
          st.horizontal := true,
          st.child := st.Checkbox(
            st.label := "Name",
            st.description := "description",
            st.message := "message",
            st.inValue := "ch1",
            st.checked --> Observer[Boolean](println)
          ),
          st.child := st.Checkbox(
            st.label := "Name2",
            st.inValue := "ch2",
            st.description := "description",
            st.message := "message",
            st.checked --> Observer[Boolean](println)
          ),
          st.checked --> Observer[Seq[String]](println)
        )
      ),
      div(
        st.RadioGroup(
          st.label := "group name",
          st.horizontal := true,
          st.child := st.RadioOption(
            st.label := "Name",
            st.description := "description",
            st.message := "message",
            st.checked --> Observer[Boolean](println),
            st.name := "radio1",
            st.inValue := "n1"
          ),
          st.child := st.RadioOption(
            st.label := "Name2",
            st.description := "description",
            st.message := "message",
            st.checked --> Observer[Boolean](println),
            st.name := "radio1",
            st.inValue := "n2",
            st.disabled := true
          ),
          st.checked --> Observer[Option[String]](println)
        )
      ),
      div(
        {
          val nav = st.NavigationGroup[Int](
            st.theme := st.NavigationStyle.NavigationMuted,
            st.child := st.NavigationItem(1)(st.label := "111"),
            st.child := st.NavigationItem(2)(st.label := "222")
          )
          List(
            nav.root,
            div(
              child <-- nav.value.toObservable.map(i => div(i.toString))
            )
          )
        }
      ),
      div(
        {
          val nav = st.MenuGroup[Int](
            st.child := st.MenuItem(1)(st.label := "111"),
            st.child := st.MenuItem(2)(st.label := "222")
          )
          List(
            nav.root,
            div(
              child <-- nav.value.toObservable.map(i => div(i.toString))
            )
          )
        }
      ),
      {
        val open = button(
          typ := "button",
          "open modal"
        )
        div(
          open,
          st.Modal(
            st.child := (p("blah blah"): HtmlElement),
            st.title := "title title title",
            st.active <-- open.events(onClick).mapToUnit.map(_ => true)
          )
        )
      },
      div(
        st.Banner(
          st.child := (span("something soem"): HtmlElement)
        )
      ),
      div(
        but
      )
    )

  }
}

object main extends App {
  documentEvents(_.onDomContentLoaded)
    .map(_ => render(dom.document.querySelector("#app"), Demo.root()))
    .mapToUnit
    .addObserver(Observer.empty)(unsafeWindowOwner)

}
