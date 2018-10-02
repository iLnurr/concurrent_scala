package ch2

import ConcurrentBiMap._

object ConcurrentBiMapTest extends App{
  val map = new ConcurrentBiMap[String, String]()

  val range = 1 to 1000000

  thread(range.foreach(e => map.put(s"key 1 - $e", s"value 1 - $e")))
  thread(range.foreach(e => map.put(s"key 2 - $e", s"value 2 - $e")))
  thread(range.foreach(e => map.put(s"key 3 - $e", s"value 3 - $e")))

}
