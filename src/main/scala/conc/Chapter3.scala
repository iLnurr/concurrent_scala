package conc

import java.util.concurrent.Executor

object Chapter3 {

  /**
    * Реализуйте собственный класс ExecutionContext с именем PiggybackContext,
    * выполняющий объекты Runnable в том же потоке, где вызывается метод
    * execute. Объект Runnable, выполняемый классом PiggybackContext, должен
    * иметь возможность сам вызывать метод execute, при этом все исключения
    * должны обрабатываться правильно.
    */
  object Ex1 {

    class PiggybackContext extends Executor {
      override def execute(command: Runnable): Unit = command.run()
    }

  }
}
