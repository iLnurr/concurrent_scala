package conc

object ChEx9Test extends App {
  def first: () ⇒ String = {
    log("first begin")
    Thread.sleep(1000)
    log("first end")
    () ⇒ "first"
  }
  def sec: () ⇒ String = {
    log("sec begin")
    Thread.sleep(100)
    log("sec end")
    () ⇒ "sec"
  }
  def thrd: () ⇒ String = {
    log("third begin")
    Thread.sleep(2000)
    log("third end")
    () ⇒ "thrd"
  }
  def frth: () ⇒ String = {
    log("frth begin")
    Thread.sleep(100)
    log("frth end")
    () ⇒ "frth"
  }

  val tasks: Seq[() ⇒ String] = Seq(first, sec, thrd, frth)

  import Chapter4.Ex9._
  scatterGather(tasks)
}
