package ch2

import ConcurrentBiMap._

object ConcurrentBiMapTest extends App{
  val map = new ConcurrentBiMap[String, String]()

  val range = 1 to 1000000

  val begin = System.currentTimeMillis()

  val t1 = thread(range.foreach(e => map.put(s"key 1 - $e", s"value 1 - $e")))
  val t2 = thread(range.foreach(e => map.put(s"key 2 - $e", s"value 2 - $e")))
  val t3 = thread(range.foreach(e => map.put(s"key 3 - $e", s"value 3 - $e")))

  t1.join()
  t2.join()
  t3.join()

  val t4 = thread(range.foreach{ e =>
    val k = s"key 1 - $e"
    val v = s"value 1 - $e"
    map.replace(k,v,v,k)
  })

  val t5 = thread(range.foreach{ e =>
    val k = s"key 2 - $e"
    val v = s"value 2 - $e"
    map.replace(k,v,v,k)
  })

  val t6 = thread(range.foreach{ e =>
    val k = s"key 3 - $e"
    val v = s"value 3 - $e"
    map.replace(k,v,v,k)
  })

  t4.join()
  t5.join()
  t6.join()

  val end = System.currentTimeMillis()

  log(s"${end - begin}")

}
