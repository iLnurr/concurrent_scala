package conc

import Chapter5._

object Ch5Ex2Test extends App {
  println(alphanumericWithSpaceProbability(0.05).take(16).mkString)
  println(alphanumericWithSpaceProbability(0.5).take(16).mkString)
  println(alphanumericWithSpaceProbability(100).take(16).mkString)
}
