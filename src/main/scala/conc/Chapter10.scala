package conc

import io.reactors._

object Chapter10 {
  /**
    * Определите метод twice, принимающий целевой канал и возвращающий другой канал,
    * который дважды посылает каждое событие в целевой канал.
    * def twice[T](target: Channel[T]): Channel[T]
    */
  def twice[T: Arrayable](target: Channel[T])
                         (implicit system: ReactorSystem): Channel[T] = {
    val prototype = Reactor[T] { self ⇒
      self.main.events onEvent { t ⇒
        println(s"twice proxy channel catch event= `$t`")
        target ! t
        target ! t
      }
    }
    system.spawn(prototype)
  }
}
