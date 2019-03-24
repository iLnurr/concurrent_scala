package conc

import io.reactors._
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration._

object Chapter10 {
  /**
    * Определите метод twice, принимающий целевой канал и возвращающий другой канал,
    * который дважды посылает каждое событие в целевой канал.
    * def twice[T](target: Channel[T]): Channel[T]
    */
  def twice[T: Arrayable](target: Channel[T])
                         (implicit system: ReactorSystem): Channel[T] = {
    system.spawn(Reactor[T] { self ⇒
      self.main.events onEvent { t ⇒
        println(s"twice proxy channel catch event= `$t`")
        target ! t
        target ! t
      }
    })
  }

  /**
    * Определите метод throttle, снижающий пропускную способность целевого
    * канала до определенного числа событий в единицу времени.
    * def throttle[T](target: Channel[T]): Channel[T]
    * Подсказка: используйте службу Clock и прием композиции потоков событий.
    */

  def throttle[T: Arrayable](target: Channel[T], duration: Duration = 2.seconds)
                            (implicit system: ReactorSystem): Channel[T] = {
    system.spawn(Reactor[T] { self ⇒
      val buffer = ArrayBuffer[T]()
      self.main.events onEvent { t ⇒
        buffer.append(t)
      }
      system.clock.periodic(duration) on {
        println("throttle")
        buffer.headOption.foreach{ t ⇒
          target ! t
          buffer.remove(0)
        }
      }
    })
  }

  /**
    * Реализуйте ненадежный широковещательный протокол, доставляющий события нескольким адресатам.
    * Метод broadcast должен реализовать следующий интерфейс,
    * где возвращаемый канал должен пересылать получаемые события во все целевые каналы:
    * def broadcast(targets: Seq[Channel[T]]): Channel[T]
    */
  def broadcast[T: Arrayable](targets: Seq[Channel[T]])
                             (implicit system: ReactorSystem): Channel[T] = {
    system.spawn(Reactor[T] { self ⇒
      self.main.events onEvent { t ⇒
        targets.foreach(_ ! t)
      }
    })
  }
}
