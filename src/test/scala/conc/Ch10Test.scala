package conc

import Chapter10._
import io.reactors._

object Ch10Test extends App {
  implicit val testSystem: ReactorSystem = ReactorSystem.default("test-system")

  val stringReactor = Reactor[String] { self =>
    self.main.events onEvent { s ⇒
      println("event received to StringReactor")
      println(s)
    }

  }
  val stringChannel = testSystem.spawn(stringReactor)

  twice(stringChannel) ! "Test"

  Thread.sleep(10000)
}
