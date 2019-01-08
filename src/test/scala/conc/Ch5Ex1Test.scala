package conc

import Chapter5._

object Ch5Ex1Test extends App {
  val index = 1000000
  val all = (1 to index).map { _ =>
    val l = timed(randomLong)
    println(l)
    l
  }
  val avg = all.seq.sum / index
  println()
  println(s"avg = $avg")
}
