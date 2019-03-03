package conc

import scala.collection.immutable.{Range, Stream}
import scala.collection.parallel.ParSeq
import scala.util.Random

object Chapter5 {
  def randomLong: Long = Math.random().toLong + 256L

  /**
    * Подсчитайте количество пробелов в случайно сгенерированной строке,
    * где вероятность появления пробела в каждой позиции задается параметром p.
    * Используйте параллельный метод foreach. Постройте график зависимости
    * продолжительности времени выполнения от величины параметра p.
    */
  def alphanumericWithSpaceProbability(p: BigDecimal) = {
    def alphanumeric: Stream[Char] = {
      val chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
      val charsWithSpaces = Random.shuffle((" " * (p* BigDecimal(chars.length)).toInt + chars).split("").toList).mkString
      def nextAlphaNum: Char = {
        charsWithSpaces.charAt(Random.nextInt(charsWithSpaces.length))
      }

      Stream.continually(nextAlphaNum)
    }
    alphanumeric
  }

  /** Реализуйте программу параллельного отображения множества Мандельброта (Mandelbrot). */
  //https://github.com/non/spire/blob/master/examples/src/main/scala/spire/example/mandelbrot.scala

  trait Point[X,Y] {
    def x: X
    def y: Y
    def mandelbrot(): String
  }

  import math._
  case class Complex(x: BigDecimal, y: BigDecimal) extends Point[BigDecimal,BigDecimal] {
    def +(that: Complex) = Complex(x + that.x, y + that.y)
    def *(that: Complex) = Complex(x * that.x- y * that.y, x * that.y + that.x * y)
    def mandelbrot(): String = {
      def calc: Boolean = {
        val p = sqrt {
          (y * y + (x - 0.25) * (x - 0.25)).doubleValue()
        }
        val pc = 0.5 - 0.5 * cos{
          atan2(y.doubleValue(), (x - 0.25).doubleValue())
        }
        p <= pc // https://ru.wikipedia.org/wiki/Множество_Мандельброта  оптимизация
      }
      if (calc) " * " else " . "
    }
  }

  case class SimplePoint(x: Int, y: Int) extends Point[Int,Int] {
    def mandelbrot(): String = {
      def calc: Boolean = y == 0 || x / y == 1 // dummy
      if (calc) " * " else " . "
    }
  }

  case class Line[X,Y](points: ParSeq[Point[X,Y]])

  case class Matrix[X,Y](lines: Seq[Line[X,Y]]) {
    def printFun(fun: Point[X,Y] ⇒ String): Unit = {
      lines.foreach{l ⇒
        println()
        print(l.points.map(fun).mkString(""))
      }
    }
  }
  object Matrix {
    def simple(limit: Int = 100, begin: Int = 0): Matrix[Int,Int] = Matrix {
      val range = begin until (begin + limit)
      for {
        x ← range
      } yield {
        Line(for {
          y ← range.par
        } yield {
          SimplePoint(x,y)
        })
      }
    }

    def complex(limit: Int = 1000, begin: Int = 0, scale: Double = 0.001): Matrix[BigDecimal,BigDecimal] = Matrix {
      val range = (begin until (begin + limit)).map(int ⇒ BigDecimal(scale) * int)
      for {
        x ← range
      } yield {
        Line(for {
          y ← range.par
        } yield {
          Complex(x,y)
        })
      }
    }
  }
}
