package conc

import conc.Chapter3.Ex6.SyncConcurrentMap

object SyncConcurrentMapTest extends App {
  val m = new SyncConcurrentMap[Int, String]()

  val t = (1 to 100).map((i) => thread {
    (1 to 100).foreach {
      (k) => {
        val v = s"${Thread.currentThread().getName}"
        m.put(k, v)
        log(s"-> ($k,$v)")
      }
    }
  })

  Thread.sleep(100)

  for ((k, v) <- m) {
    log(s"<- ($k,$v)")
  }

  t.foreach(_.join)
}
