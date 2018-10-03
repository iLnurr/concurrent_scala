package ch2

import ConcurrentBiMap._
import ch2.PriorityTaskPoolExtended._

object ConcurrentBiMapTestUsePool extends App{
  val map = new ConcurrentBiMap[String, String]()

  val range = 1 to 1000000

  val begin = System.currentTimeMillis()

  val pool = new PriorityTaskPoolExtended(3)

  pool.asynchronous(11)(range.foreach(e => map.put(s"key 1 - $e", s"value 1 - $e")))
  pool.asynchronous(11)(range.foreach(e => map.put(s"key 2 - $e", s"value 2 - $e")))
  pool.asynchronous(11)(range.foreach(e => map.put(s"key 3 - $e", s"value 3 - $e")))

  pool.asynchronous(1)(range.foreach{ e =>
    val k = s"key 1 - $e"
    val v = s"value 1 - $e"
    map.replace(k,v,v,k)
  })

  pool.asynchronous(1)(range.foreach{ e =>
    val k = s"key 2 - $e"
    val v = s"value 2 - $e"
    map.replace(k,v,v,k)
  })

  pool.asynchronous(1)(range.foreach{ e =>
    val k = s"key 3 - $e"
    val v = s"value 3 - $e"
    map.replace(k,v,v,k)
  })

  val end = System.currentTimeMillis()

  log(s"${end - begin} millis")

}