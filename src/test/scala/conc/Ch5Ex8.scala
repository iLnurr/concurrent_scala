package conc

import conc.Chapter5.BinomialHeap

object Ch5Ex8 extends App {
  val heap = new BinomialHeap[Int]()

  println("Add")
  println(heap.insert(1))
  println(heap.insert(2))
  println(heap.insert(3))

  println("Remove")
  println(heap.remove)
  println(heap.remove)
  println(heap.remove)

}
