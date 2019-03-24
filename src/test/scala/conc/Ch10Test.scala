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

  private def twiceTest(): Unit = {
    twice(stringChannel) ! "Test"

    Thread.sleep(1000)
  }

  private def throttleTest(): Unit = {
    val throttled = throttle(stringChannel)

    (1 to 10).foreach(i ⇒ throttled ! i.toString)

    Thread.sleep(30000)
  }

}
