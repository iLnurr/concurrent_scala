package conc

import conc.Chapter3.Ex3.ConcurrentSortedList

object ConcurrentSortedListTest extends App {
  val csl = new ConcurrentSortedList[Int]()


  (1 to 100).map(i => thread {
    Thread.sleep((Math.random() * 100).toInt)
    for (i <- 1 to 1000) {
      Thread.sleep((Math.random() * 10).toInt)
      csl.add((math.random * 100 + i).toInt)
    }
  }
  ).foreach(_.join)

  log(s"length = ${csl.iterator.length}")

  var prev = 0
  var length = 0
  for (a <- csl.iterator) {
    log(a.toString)
    if (prev > a) throw new Exception(s"$prev > $a")
    prev = a
    length += 1
  }

  if (csl.iterator.length != length) throw new Exception(s"${csl.iterator.length} != $length")

  log(s"length = ${csl.iterator.length} ($length)")
}
