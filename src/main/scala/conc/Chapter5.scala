package conc

import scala.collection.immutable.{Range, Stream}
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
}
