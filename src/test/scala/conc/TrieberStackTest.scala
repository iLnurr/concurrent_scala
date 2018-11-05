package conc

import conc.Chapter3.Ex2.TreiberStack

object TrieberStackTest extends App {
  val s = new TreiberStack[Int]

  val t1 = thread {
    for (i <- 1 to 10) {
      s.push(i)
      Thread.sleep(1)
    }
  }

  val t2 = thread  {
    for (i <- 1 to 10) {
      s.push(i*10)
      Thread.sleep(1)
    }
  }

  t1.join()
  t2.join()

  for (i <- 1 to 20)
    log(s"s[$i] = ${s.pop()}")
}
