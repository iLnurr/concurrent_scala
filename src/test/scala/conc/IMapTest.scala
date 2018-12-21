package conc

import conc.Chapter4.Ex7.{AlreadyAssocKeyException, IMapImpl}

import scala.concurrent.Await
import scala.concurrent.duration.Duration

object IMapTest extends App {
  val m = new IMapImpl[Int, String]()

  (1 to 100).map(i => thread {
    try {
      m.update(1, Thread.currentThread().getName)
    } catch {
      case known: AlreadyAssocKeyException ⇒ //NOP
      case other ⇒ throw other
    }
  })

  val result = Await.result(m(1), Duration.Inf)

  (1 to 100).map(i => thread {
    val l = Await.result(m(1), Duration.Inf)
    assert(result == l, "Map must contain only one key -> value pair")
    log(l)
  })

  assert(m.underlying.size == 1, "Map must contain only one key -> value pair")

}
