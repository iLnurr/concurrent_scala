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

  def crdtTest()= {
    type T = GCrdtReactorStateBased
    def counterTest(r1: T, r2: T, r3: T) = {
      r1.broadcastTo(r2,r3)
      r2.broadcastTo(r1,r3)
      r3.broadcastTo(r2,r1)

      r1.increment(1)
      r2.increment(2)
      r3.increment(4)
      // Initial result works
      assert(
        r1.value == 1 && r2.value == 2 && r3.value == 4,
        s"""
           |${r1.value} != 1, ${r2.value} != 2, ${r3.value} != 4
         """.stripMargin
      )

      Thread.sleep(4000)

      assert(
        r1.counterSnap(0) == 1 && r1.counterSnap(1) == 2 && r1.counterSnap(2) == 4,
        s"""
           |${r1.counterSnap(0)} != 1, ${r1.counterSnap(1)} != 2, ${r1.counterSnap(2)} != 4
         """.stripMargin
      )

      // Merged result works
      assert(
        r1.value == 7 && r2.value == 7 && r3.value == 7,
        s"""
           |${r1.value} != 7 || ${r2.value} != 7 || ${r3.value} != 7
         """.stripMargin
      )

      println(r1.counterSnap)
      println(r2.counterSnap)
      println(r3.counterSnap)
    }

    counterTest(new GCrdtReactorStateBased(0),new GCrdtReactorStateBased(1),new GCrdtReactorStateBased(2))
  }

  crdtTest()

}
