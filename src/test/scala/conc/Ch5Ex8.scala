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

  println("Merge")
//  val first = new BinomialHeap[Int]().insertAll(Seq(1,2,3,4,5):_*)
//  val sec = new BinomialHeap[Int]().insertAll(Seq(7,8,9):_*)
//  println(first)
//  println(sec)
//  println(first.merge(sec))
//
//  println(first.par.min)



}
