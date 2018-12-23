package conc

import conc.Chapter4.Ex11._

import scala.concurrent.Await
import scala.concurrent.duration.Duration


object Ch4Ex11Test extends App {
  val a = new DAG("a")
  val b = new DAG("b")
  val c = new DAG("c")
  val d = new DAG("d")
  val e = new DAG("e")

  a.edges += b
  b.edges += c
  b.edges += d
  c.edges += e
  d.edges += e

  val f = fold[String, String](a, { (value, edges) =>
    println(value)
    Thread.sleep(1000)
    val combined = if (edges.nonEmpty) edges.reduce(_ + _) else ""
    value + combined
  })

  println(Await.result(f, Duration.Inf))
}
