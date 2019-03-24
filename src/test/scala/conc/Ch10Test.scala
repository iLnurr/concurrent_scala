package conc

import Chapter10._
import io.reactors._

object Ch10Test extends App {
  implicit val testSystem: ReactorSystem = ReactorSystem.default("test-system")

  def stringReactor(name: String) = Reactor[String] { self =>
    self.main.events onEvent { s ⇒
      println(s"$name stringReactor receive event: $s")
    }

  }
  val stringChannel1 = testSystem.spawn(stringReactor("first"))
  val stringChannel2 = testSystem.spawn(stringReactor("second"))
  val stringChannel3 = testSystem.spawn(stringReactor("third"))

  val channels = Seq(stringChannel1,stringChannel2,stringChannel3)

  def twiceTest(): Unit = {
    twice(stringChannel1) ! "Test"

    Thread.sleep(1000)
  }

  def throttleTest(): Unit = {
    val throttled = throttle(stringChannel1)

    (1 to 10).foreach(i ⇒ throttled ! i.toString)

    Thread.sleep(30000)
  }

  def broadcastTest(channels: Seq[Channel[String]]) = {
    val broadcastChannel = broadcast(channels)

    broadcastChannel ! "broadcast message"

    Thread.sleep(5000)
  }

  broadcastTest(channels)

}
