package ru.otus.module3

import ru.otus.module3.zio_homework.config.{AppConfig, Configuration}

import scala.language.postfixOps
import zio.{Clock, Console, Random, ZIO, ZLayer, durationInt}

import java.io.IOException
import java.util.concurrent.TimeUnit

package object zio_homework {
  /**
   * 1.
   * Используя сервисы Random и Console, напишите консольную ZIO программу которая будет предлагать пользователю угадать число от 1 до 3
   * и печатать в консоль угадал или нет. Подумайте, на какие наиболее простые эффекты ее можно декомпозировать.
   */



  lazy val guessProgram = game

  val game: ZIO[Any, IOException, Any] =  for {
    _ <- Console.printLine("угадай число от 1 до 3")
    n <- Console.readLine
    r <- Random.nextIntBetween(1, 4)
    _ <- if (n.toInt == r) Console.printLine("угадали") else game
  } yield ()

  /**
   * 2. реализовать функцию doWhile (общего назначения), которая будет выполнять эффект до тех пор, пока его значение в условии не даст true
   * 
   */

  def doWhile(zio: ZIO[Any, Any, Boolean]): ZIO[Any, Any, Unit] = for {
    r <- zio
    _ <- doWhile(zio).unless(r)
  } yield ()

  /**
   * 3. Реализовать метод, который безопасно прочитает конфиг из переменных окружения, а в случае ошибки вернет дефолтный конфиг
   * и выведет его в консоль
   * Используйте эффект "Configuration.config" из пакета config
   */


  def loadConfigOrDefault = Configuration.config.orElse(ZIO.succeed(AppConfig("localhost", "8080")))


  /**
   * 4. Следуйте инструкциям ниже для написания 2-х ZIO программ,
   * обратите внимание на сигнатуры эффектов, которые будут у вас получаться,
   * на изменение этих сигнатур
   */


  /**
   * 4.1 Создайте эффект, который будет возвращать случайным образом выбранное число от 0 до 10 спустя 1 секунду
   * Используйте сервис zio Random
   */
  lazy val eff = Random.nextIntBetween(0, 11) <* ZIO.sleep(1 second)

  /**
   * 4.2 Создайте коллукцию из 10 выше описанных эффектов (eff)
   */
  lazy val effects = List.fill(10)(eff)

  
  /**
   * 4.3 Напишите программу которая вычислит сумму элементов коллекции "effects",
   * напечатает ее в консоль и вернет результат, а также залогирует затраченное время на выполнение,
   * можно использовать ф-цию printEffectRunningTime, которую мы разработали на занятиях
   */

  lazy val app = printEffectRunningTime(sum)

  lazy val sum = for {
    summ <- ZIO.foldLeft(effects)(0)((s, a) => a.map(_ + s))
    _ <- Console.printLine(s"summ = $summ")
  } yield summ

  def printEffectRunningTime[R, E, A](zio: ZIO[R, E, A]): ZIO[R, E, A] = for{
    start <- Clock.currentTime(TimeUnit.SECONDS)
    r <- zio
    end <- Clock.currentTime(TimeUnit.SECONDS)
    _ <- Console.printLine(s"Running time ${end - start}").orDie
  } yield r
  /**
   * 4.4 Усовершенствуйте программу 4.3 так, чтобы минимизировать время ее выполнения
   */

  lazy val appSpeedUp = printEffectRunningTime(for {
    summ <- ZIO.reduceAllPar(ZIO.succeed(0), effects)(_+_)
    _ <- Console.printLine(s"summ = $summ")
  } yield summ)


  /**
   * 5. Оформите ф-цию printEffectRunningTime разработанную на занятиях в отдельный сервис, так чтобы ее
   * можно было использовать аналогично zio.Console.printLine например
   */
trait RunningTime {
    def printEffectRunningTime[R, E, A](zio: ZIO[R, E, A]): ZIO[R, E, A] = for{
      start <- Clock.currentTime(TimeUnit.SECONDS)
      r <- zio
      end <- Clock.currentTime(TimeUnit.SECONDS)
      _ <- Console.printLine(s"Running time ${end - start}").orDie
    } yield r
  }
object RunningTime {
  def printEffectRunningTime[R, E, A](zio: ZIO[R, E, A]): ZIO[R with RunningTime, E, A] = ZIO.serviceWithZIO[RunningTime](_.printEffectRunningTime(zio))
}
   /**
     * 6.
     * Воспользуйтесь написанным сервисом, чтобы создать эффект, который будет логировать время выполнения программы из пункта 4.3
     *
     * 
     */

  lazy val appWithTimeLogg: ZIO[Any with RunningTime, IOException, Int] = RunningTime.printEffectRunningTime(sum)

  /**
    * 
    * Подготовьте его к запуску и затем запустите воспользовавшись ZioHomeWorkApp
    */

  lazy val runApp: ZIO[Any, IOException, Int] = appWithTimeLogg.provideLayer(ZLayer.succeed(new RunningTime {}))

}
