package conc

import conc.Cache.Cache

object CacheTest extends App {
  val ins = new Cache[Int, Int]()

  val f1: Int ⇒ Int = 5 + _
  val f2: Int ⇒ Int = 15 - _

  ins.cache(f1)(10)
  ins.cache(f1)(10)
  ins.cache(f1)(9)

}
