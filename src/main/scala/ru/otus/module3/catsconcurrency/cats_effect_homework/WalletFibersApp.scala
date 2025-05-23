package ru.otus.module3.catsconcurrency.cats_effect_homework

import cats.effect.{IO, IOApp}
import cats.implicits._

import scala.concurrent.duration.{DurationInt, FiniteDuration}

// Поиграемся с кошельками на файлах и файберами.

// Нужно написать программу где инициализируются три разных кошелька и для каждого из них работает фоновый процесс,
// который регулярно пополняет кошелек на 100 рублей раз в определенный промежуток времени. Промежуток надо сделать разный, чтобы легче было наблюдать разницу.
// Для определенности: первый кошелек пополняем раз в 100ms, второй каждые 500ms и третий каждые 2000ms.
// Помимо этих трёх фоновых процессов (подсказка - это файберы), нужен четвертый, который раз в одну секунду будет выводить балансы всех трех кошельков в консоль.
// Основной процесс программы должен просто ждать ввода пользователя (IO.readline) и завершить программу (включая все фоновые процессы) когда ввод будет получен.
// Итого у нас 5 процессов: 3 фоновых процесса регулярного пополнения кошельков, 1 фоновый процесс регулярного вывода балансов на экран и 1 основной процесс просто ждущий ввода пользователя.

// Можно делать всё на IO, tagless final тут не нужен.

// Подсказка: чтобы сделать бесконечный цикл на IO достаточно сделать рекурсивный вызов через flatMap:
// def loop(): IO[Unit] = IO.println("hello").flatMap(_ => loop())
object WalletFibersApp extends IOApp.Simple {

  def repeat(io : IO[Unit], sleep: FiniteDuration) : IO[Nothing] = io >> IO.sleep(sleep) >> repeat(io, sleep)

  def run: IO[Unit] = app

  val app =     for {
    _ <- IO.println("Press any key to stop...")
    wallet1 <- Wallet.fileWallet[IO]("1")
    wallet2 <- Wallet.fileWallet[IO]("2")
    wallet3 <- Wallet.fileWallet[IO]("3")
    // todo: запустить все файберы и ждать ввода от пользователя чтобы завершить работу
    f1 <- repeat(wallet1.topup(100), 100.millis).start
    f2 <- repeat(wallet2.topup(100), 500.millis).start
    f3 <- repeat(wallet3.topup(100), 2000.millis).start
    p <- repeat(for {
      b1 <- wallet1.balance
      b2 <- wallet2.balance
      b3 <- wallet3.balance
    } yield println(s"wallet1 = $b1, wallet2 = $b2, wallet3 = $b3"), 1.second).start
    _ <- IO.readLine
  } yield ()

}
