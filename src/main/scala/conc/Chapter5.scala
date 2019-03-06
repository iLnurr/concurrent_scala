package conc

import java.io.FileOutputStream
import java.util.concurrent.atomic.{AtomicReference, AtomicReferenceArray}

import scala.annotation.tailrec
import scala.collection.immutable.{Range, Stream}
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.collection.parallel.{IterableSplitter, ParIterable, ParSeq, SeqSplitter}
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


  /**
    * Биномиальная куча (binomial heap),
    * описанная в докторской диссертации «Purely Functional Data Structures» Криса Окасаки (Chris Okasaki),
    * – это неизменяемая структура данных,
    * которая эффективно реализует приоритетную очередь с четырьмя основными операциями: вставка элемента,
    * поиск наименьшего элемента, удаление наименьшего элемента и слияние двух биномиальных куч:
    * Реализуйте класс BinomialHeap.
    * Затем реализуйте сплиттеры и комбинаторы для биномиальной кучи и переопределите операцию par.
    */
  class BinomialHeap[T] extends ParIterable[T] {
    private val underlying = new AtomicReference[ArrayBuffer[T]](ArrayBuffer.empty[T])
    def insert(x: T): BinomialHeap[T] = {
      @tailrec def retry(): BinomialHeap[T] = {
        val arr = underlying.get()
        val newArr = arr :+ x
        if (!underlying.compareAndSet(arr, newArr)) retry() else this
      }
      retry()
    }
    def remove(implicit ord: Ordering[T]): (T, BinomialHeap[T]) = {
      @tailrec def retry(): (T,BinomialHeap[T]) = {
        val arr = underlying.get()
        val toRm = arr.min
        val newArr = arr -= toRm
        if (!underlying.compareAndSet(arr, newArr)) retry() else (toRm,this)
      }
      retry()
    }
    def smallest(implicit ord: Ordering[T]): T = {
      underlying.get().min
    }
    def merge(that: BinomialHeap[T]): BinomialHeap[T] = {
      @tailrec def retry(): BinomialHeap[T] = {
        val thisArr = underlying.get()
        val thatArr = that.underlying.get()
        val newArr = thisArr ++ thatArr
        if (!underlying.compareAndSet(thisArr, newArr)) retry() else this
      }
      retry()
    }

    override def seq: Iterable[T] = underlying.get()

    override def splitter: IterableSplitter[T] = {
      val arr = underlying.get()
      new TSplitter[T](arr, 0, arr.size)
    }

    override def size: Int = underlying.get().size
  }
  class TSplitter[T](val arr: ArrayBuffer[T], var i: Int, val limit: Int) extends SeqSplitter[T] {
    override def dup: SeqSplitter[T] = new TSplitter[T](arr, i, limit)
    override def split: Seq[SeqSplitter[T]] = {
      val rem = remaining
      if (rem >= 2) psplit(rem / 2, rem - rem / 2) else Seq(this)
    }
    override def psplit(sizes: Int*): Seq[SeqSplitter[T]] = {
      val result = for (sz <- sizes) yield {
        val newLimit = (i + sz).min(limit)
        val newSplitter = new TSplitter[T](arr, i, newLimit)
        i = newLimit
        newSplitter
      }

      if (i == limit) result else result :+ new TSplitter(arr, i, limit)
    }
    override def remaining: Int = limit - i
    override def hasNext: Boolean = i < limit
    override def next(): T = {
      val r = arr(i)
      i += 1
      r
    }
  }

  /**
    * Реализуйте метод parallelBalanceParentheses , возвращающий true , если
    * каждой открывающей круглой скобке в строке соответствует закрываю-
    * щая круглая скобка (и наоборот). То есть при просмотре строки слева на-
    * право количество встреченных открывающих скобок всегда должно быть
    * больше или равно количеству встреченных закрывающих скобок, и общее
    * количество открывающих скобок в конце должно быть равно общему ко-
    * личеству закрывающих скобок. Например, для строки 0(1)(2(3))4 метод
    * parallelBalanceParentheses должен вернуть true , а для строк 0)2(1(3) и 0((1)2 –
    * false . Используйте метод aggregate .
    */

  def parallelBalanceParentheses(s: String): Boolean = {
    import scala.annotation.tailrec
    @tailrec def recur(list: List[Int]): List[Int] = list match {
      case Nil => Nil
      case _ => {
        val negs = list.takeWhile(_ < 0)
        println(s"negs \n$negs")
        val other = list.dropWhile(_ < 0)
        println(s"other \n$other")
        require(negs.nonEmpty, "negs must be nonEmpty")
        require(other.nonEmpty && other.size >= negs.size, "poses must be ge that negs")
        val otherAfterPosDropFromLeft = other.dropWhile(_ > 0)
        println(s"otherAfterPosDropFromLeft \n$otherAfterPosDropFromLeft")
        val droppedFromLeftSize = other.size - otherAfterPosDropFromLeft.size
        println(s"droppedFromLeftSize \n$droppedFromLeftSize")
        //  require(otherAfterPosDropFromLeft.size >= droppedFromLeftSize, s"otherAfterPosDropFromLeft.size:${otherAfterPosDropFromLeft.size} must be >= droppedFromLeftSize:$droppedFromLeftSize")
        val needToDropFromRight = negs.size - droppedFromLeftSize
        val rightDropped = otherAfterPosDropFromLeft.takeRight(needToDropFromRight)
        require(rightDropped.size == needToDropFromRight && rightDropped.forall(_ > 0), "rightDropped.size == needToDropFromRight && rightDropped.forall(_ > 0)")
        val otherAfterPosDropFromRight = otherAfterPosDropFromLeft.dropRight(needToDropFromRight)
        println(s"otherAfterPosDropFromRight \n$otherAfterPosDropFromRight")
        val droppedFromRight = otherAfterPosDropFromLeft.size - otherAfterPosDropFromRight.size
        println(s"droppedFromRight \n$droppedFromRight")
        require(droppedFromLeftSize + droppedFromRight == negs.size, s"droppedFromLeftSize:$droppedFromLeftSize + droppedFromRight$droppedFromRight must be eq negs.size:${negs.size}")
        val rest = otherAfterPosDropFromRight
        println(s"rest \n$rest")
        recur(rest)
      }
    }
    val result = s.aggregate(List.empty[Int])(
      (acc, char) => char match {
        case '(' => -1 :: acc
        case ')' => 1 :: acc
        case _ => acc
      },
      (acc1,acc2) => acc1 ++ acc2
    ).reverse

    result match {
      case Nil => true
      case List(_) => false
      case head :: tail if head == 1 || tail.last == -1 || result.size % 2 != 0 => false
      case unstable if unstable.sum != 0 => false
      case other =>
        recur(other).isEmpty
    }
  }

  val s1 = "0(1)(2(3))4"
  val s2 = "0)2(1(3)"
  val s3 = "0((1)2"
}
