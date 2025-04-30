package ru.otus.module3.catsconcurrency.cats_effect_homework

import cats.effect.Sync
import cats.implicits._
import Wallet._
import cats.data.EitherT

import java.nio.file.{Files, Paths, StandardOpenOption}
import scala.util.Try

// DSL управления электронным кошельком
trait Wallet[F[_]] {
  // возвращает текущий баланс
  def balance: F[BigDecimal]
  // пополняет баланс на указанную сумму
  def topup(amount: BigDecimal): F[Unit]
  // списывает указанную сумму с баланса (ошибка если средств недостаточно)
  def withdraw(amount: BigDecimal): F[Either[WalletError, Unit]]
}

// Игрушечный кошелек который сохраняет свой баланс в файл
// todo: реализовать используя java.nio.file._
// Насчёт безопасного конкуррентного доступа и производительности не заморачиваемся, делаем максимально простую рабочую имплементацию. (Подсказка - можно читать и сохранять файл на каждую операцию).
// Важно аккуратно и правильно завернуть в IO все возможные побочные эффекты.
//
// функции которые пригодятся:
// - java.nio.file.Files.write
// - java.nio.file.Files.readString
// - java.nio.file.Files.exists
// - java.nio.file.Paths.get
final class FileWallet[F[_]: Sync](id: WalletId) extends Wallet[F] {
  def balance: F[BigDecimal] = {
    val F = Sync[F]
    for {
      path <- F.fromTry(Try(Paths.get(id)))
      _ <- F.fromTry(Try(Files.exists(path)))
      res <- F.fromTry(Try(Files.readString(path)))
    } yield BigDecimal(res)
}
  def topup(amount: BigDecimal): F[Unit] = {
    val F = Sync[F]
    for {
      cur <- balance
      newCur <- F.pure(cur + amount)
      _ <- F.fromTry(Try(Files.writeString(Paths.get(id), newCur.toString(), StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING)))
    } yield ()
  }
  def withdraw(amount: BigDecimal): F[Either[WalletError, Unit]] = {
   val F = Sync[F]
   val r = for {
      cur <- EitherT.liftF(balance)
      newCur <- EitherT.fromEither(if (cur < amount) Left(BalanceTooLow: WalletError) else Right(cur - amount))(F)
      _ <- EitherT.fromEither(Try(Files.writeString(Paths.get(id), newCur.toString(), StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING)).fold(_ => Left(WriteError: WalletError), _ => Right()))(F)
    } yield ()
    r.value
  }
}

object Wallet {

  // todo: реализовать конструктор
  // внимание на сигнатуру результата - инициализация кошелька имеет сайд-эффекты
  // Здесь нужно использовать обобщенную версию уже пройденного вами метода IO.delay,
  // вызывается она так: Sync[F].delay(...)
  // Тайпкласс Sync из cats-effect описывает возможность заворачивания сайд-эффектов
  def fileWallet[F[_]: Sync](id: WalletId): F[Wallet[F]] = {
    val F = Sync[F]
    for {
      path <- F.fromTry(Try(Paths.get(id)))
      exist <- F.delay(Files.exists(path))
    } yield if(exist) new FileWallet[F](id) else new FileWallet[F](Files.writeString(Paths.get(id), "0", StandardOpenOption.CREATE).toString())
  }

  type WalletId = String

  sealed trait WalletError
  case object BalanceTooLow extends WalletError
  case object WriteError extends WalletError
}
