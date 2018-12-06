package conc

import scala.concurrent.{Await, Future}
import scala.io.Source
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

object Chapter4 {
  /**
    * Реализуйте программу командной строки,
    * предлагающую пользователю ввести URL некоторого веб-сайта и
    * отображающую разметку HTML этого веб-сайта.
    * Между моментом, когда пользователь нажмет клавишу ENTER и когда будет получена HTML-страница,
    * программа должна каждые 50 миллисекунд выводить символ точки.
    * При этом предельное время ожидания получения страницы составляет 2 секунды.
    * Используйте только объекты Future и Promise, и избегайте примитивов синхронизации, описанных в предыдущей главе.
    * Вы можете повторно использовать метод timeout из этой главы.
    */
  object Ex1 {
    def recurUrl(): String = try {
      println("Please type url and hit enter")
      val line = scala.io.StdIn.readLine()
      val url = RefinedUrl.unsafeFrom(line)
      url.value
    } catch {
      case _: IllegalArgumentException ⇒
        recurUrl()
    }

    def run() = {
      val urlS = recurUrl()
      val future = Future(Source.fromURL(urlS).mkString)
      val timer = mytimer(println("."),1,10)
      val html = Await.result(future, 2.seconds)
      timer.cancel()
      println(s"html:\n $html")

    }
  }

}
