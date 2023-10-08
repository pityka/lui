package example

import com.raquo.laminar.api.L._
import lui.{stack => st}
import org.scalajs.dom
object Demo {

  val firstName = st
    .TextField(
      st.label := "First Name",
    )
  val lastName = st
    .TextField(
      st.label := "Last Name"
    )
  val zipCodeValidState = Var(Option.empty[String])
  val zipCodeValidator = zipCodeValidState.writer.contramap[String](s =>
    if (s.forall(_.isDigit)) None else Some("needs integer")
  )
  val zipCode = st
    .TextField(
      st.label := "zip code",
      st.value --> zipCodeValidator,
      st.variant <-- zipCodeValidState.signal.map(
        _.map(_ => st.TextField.Warning)
      ),
      st.message <-- zipCodeValidState.signal.map(_.getOrElse(""))
    )

  val checkBananas = st.Checkbox(
    st.label := "like bananas"
  )
  val checkApples = st.Checkbox(
    st.label := "like apples"
  )
  val checkChoco = st.Checkbox(
    st.label := "like chocolate"
  )

  val formStateAlways = Signal
    .combine(
      firstName.value,
      lastName.value,
      zipCode.value,
      checkBananas.checked,
      checkApples.checked,
      checkChoco.checked
    )
    .map { case all @ (f, l, z, b, a, ch) =>
      val likes = List(b -> "bananas", a -> "apples", ch -> "chocolate")
        .filter(_._1._2)
        .map(_._2)
        .mkString(", ")
      val valid = f.nonEmpty && l.nonEmpty && z.nonEmpty && z.forall(
        _.isDigit
      ) && z.toInt > 0
      (valid, s"$f $l from $z likes $likes", all)
    }

  val counter = Var(0)

  val submitButton = st.Button(
    st.label := "Submit",
    st.variant := st.Button.Primary,
    st.value --> counter.updater[Unit]((o, _) => o + 1),
    st.disabled <-- formStateAlways.map { case (valid, _, _) => !valid }
  )

  val formStateWhenClicked =
    submitButton.click
      .withCurrentValueOf(formStateAlways)
      .withCurrentValueOf(counter.signal)

  val searchField = st
    .TextField(
      st.placeholder := "Search…",
      st.size := st.TextField.ExtraLarge
    )

  val selecteds = Var(Set.empty[String])
  val base =
    "By submitting this form you agree to eat bananas. At vero eos et accusamus et iusto odio dignissimos ducimus qui blanditiis praesentium voluptatum deleniti atque corrupti quos dolores et quas molestias excepturi sint occaecati cupiditate non provident, similique sunt in culpa qui officia deserunt mollitia animi, id est laborum et dolorum fuga."
      .toLowerCase()
      .split(" ")

  def root() = div(
    cls := "d-flex mx-auto w100 wmx12",
    st.Banner(
      st.child := (span("yaay"): HtmlElement),
      st.active <-- formStateWhenClicked.map(_._3).map {
        case (_, _, _, a, b, c) => a._2 && b._2 && c._2
      }
    ),
    div(
      // main side nav on the right can go here
      div(
        cls := "flex--item fl-shrink0 ws4 md:w-auto md:d-none print:d-none",
        div(
          cls := "ps-fixed t64 py24 px8 ws4",
          p(
            "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
          ),
          pre(
            cls := "s-code-block",
            child.text <-- formStateAlways.map(_._2)
          ),
          p(
            child.text <-- formStateWhenClicked.map(v =>
              "CLICKED " + v._2 + " " + v._4 + "x"
            )
          ),
          p(
            child.text <-- selecteds.signal.map(_.mkString(", "))
          )
        )
      )
    ),
    div(
      cls := "d-flex ps-relative t64 py24 pl48 pr48 md:pl24 sm:pl16 sm:pr16 ",
      div(
        // secondary side nav on the lft can go here
        cls := "flex--item order-last ml32 sm:d-none print:d-none"
      ),
      div(
        // main content area
        div(
          h3("Introduction"),
          p(
            "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
          ),
          p(
            "Sed ut perspiciatis unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, totam rem aperiam, eaque ipsa quae ab illo inventore veritatis et quasi architecto beatae vitae dicta sunt explicabo. Nemo enim ipsam voluptatem quia voluptas sit aspernatur aut odit aut fugit, sed quia consequuntur magni dolores eos qui ratione voluptatem sequi nesciunt. Neque porro quisquam est, qui dolorem ipsum quia dolor sit amet, consectetur, adipisci velit, sed quia non numquam eius modi tempora incidunt ut labore et dolore magnam aliquam quaerat voluptatem. Ut enim ad minima veniam, quis nostrum exercitationem ullam corporis suscipit laboriosam, nisi ut aliquid ex ea commodi consequatur? Quis autem vel eum iure reprehenderit qui in ea voluptate velit esse quam nihil molestiae consequatur, vel illum qui dolorem eum fugiat quo voluptas nulla pariatur?"
          )
        ),
        div(
          h3("Form"),
          cls := "d-flex fd-column",
          div(
            cls := "pl16",
            div(
              cls := "d-flex md:fd-column gx16",
              firstName,
              lastName,
              zipCode
            ),
            div(
              cls := "d-flex gx32",
              checkApples,
              checkBananas,
              checkChoco
            ),
            div(
              cls := "d-flex fd-row-reverse pb2",
              submitButton
            ),
            p(
              cls := "fc-black-400",
              "By submitting this form you agree to eat bananas. At vero eos et accusamus et iusto odio dignissimos ducimus qui blanditiis praesentium voluptatum deleniti atque corrupti quos dolores et quas molestias excepturi sint occaecati cupiditate non provident, similique sunt in culpa qui officia deserunt mollitia animi, id est laborum et dolorum fuga."
            )
          ),
          div(
            h3("Search bar"),
            div(
              cls := "w100",
              searchField
            ),
            div(
              cls := "d-flex fw-wrap gx8",
              children <-- searchField.value.map { keyw =>
                if (keyw.isEmpty()) Nil
                else
                  base.filter(_.contains(keyw)).toList.distinct.map { word =>
                    st.Checkbox(
                      st.label := word,
                      st.inChecked <-- selecteds.signal
                        .map(v => v.contains(word)),
                      st.checked --> selecteds.updater[Boolean] {
                        case (set, true)  => set + word
                        case (set, false) => set - word
                      }
                    ).root
                  }
              }
            )
          )
        )
      )
    )
  )
 
}

object main extends App {
  documentEvents(_.onDomContentLoaded)
    .map(_ => render(dom.document.querySelector("#app"), Demo.root()))
    .mapToUnit
    .addObserver(Observer.empty)(unsafeWindowOwner)

}
