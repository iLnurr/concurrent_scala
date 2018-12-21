package conc

import conc.Chapter4.Ex2.IVar

object Ch4Ex2Test extends App{
  val v = new IVar[String]
  (1 to 10).foreach(i => thread {
    try {
      v := s"v = ${Thread.currentThread().getName}"
    } catch {
      case e:Throwable => log(s"Error !!! ${e.getMessage}. Current value = ${v.apply}")
    }
  }
  )
}
