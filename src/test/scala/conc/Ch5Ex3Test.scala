package conc

import conc.Chapter5.Matrix

object Ch5Ex3Test extends App {
  Matrix().printFun(p ⇒ s"(${p.x};${p.y})")

  Matrix().printFun(_.mandelbrot())
}
