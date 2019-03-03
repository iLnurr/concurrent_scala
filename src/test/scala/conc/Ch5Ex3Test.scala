package conc

import conc.Chapter5.Matrix

object Ch5Ex3Test extends App {
  Matrix.simple().printFun(p ⇒ s"(${p.x};${p.y})")

  Matrix.simple().printFun(_.mandelbrot())

  Matrix.complex().printFun(p ⇒ s"(${p.x};${p.y})")

  Matrix.complex().printFun(_.mandelbrot())
}
