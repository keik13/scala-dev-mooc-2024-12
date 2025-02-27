package ru.otus.module2

import cats.Functor
import cats.implicits._

import scala.util.{Failure, Try}


package object catsHomework {

  /**
   * Простое бинарное дерево
   * @tparam A
   */
  sealed trait Tree[+A]
  final case class Branch[A](left: Tree[A], right: Tree[A])
    extends Tree[A]
  final case class Leaf[A](value: A) extends Tree[A]

  /**
   * Напишите instance Functor для объявленного выше бинарного дерева.
   * Проверьте, что код работает корректно для Branch и Leaf
   */

   implicit lazy val treeFunctor = new Functor[Tree] {
     def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
       case Branch(left, right) => Branch(map(left)(f), map(right)(f))
       case Leaf(value) => Leaf(f(value))
     }
   }

  def doMath[F[_] : Functor](start: F[Int]): F[Int] = start.map(_ + 1)

  /**
   * Monad абстракция для последовательной
   * комбинации вычислений в контексте F
   * @tparam F
   */
  trait Monad[F[_]]{
    def flatMap[A,B](fa: F[A])(f: A => F[B]): F[B]
    def pure[A](v: A): F[A]

    def map[A,B](fa: F[A])(f: A => B): F[B] = flatMap(fa)(v => pure(f(v)))
  }


  /**
   * MonadError расширяет возможность Monad
   * кроме последовательного применения функций, позволяет обрабатывать ошибки
   * @tparam F
   * @tparam E
   */
  trait MonadError[F[_], E] extends Monad[F]{
    // Поднимаем ошибку в контекст `F`:
    def raiseError[A](e: E): F[A]

    // Обработка ошибки, потенциальное восстановление:
    def handleErrorWith[A](fa: F[A])(f: E => F[A]): F[A]

    // Обработка ошибок, восстановление от них:
    def handleError[A](fa: F[A])(f: E => A): F[A]

    // Test an instance of `F`,
    // failing if the predicate is not satisfied:
    def ensure[A](fa: F[A])(e: E)(f: A => Boolean): F[A]
  }

  /**
   * Напишите instance MonadError для Try
   */

   implicit lazy val tryME: MonadError[Try, Throwable] = new MonadError[Try, Throwable] {

     override def raiseError[A](e: Throwable): Try[A] = Failure(e)

     override def handleErrorWith[A](fa: Try[A])(f: Throwable => Try[A]): Try[A] = fa.recoverWith(f(_))

     override def handleError[A](fa: Try[A])(f: Throwable => A): Try[A] = fa.recover(f(_))

     override def ensure[A](fa: Try[A])(e: Throwable)(f: A => Boolean): Try[A] = fa.filter(f).orElse(Failure(e))

     override def flatMap[A, B](fa: Try[A])(f: A => Try[B]): Try[B] = fa.flatMap(f)

     override def pure[A](v: A): Try[A] = Try(v)
   }

  /**
   * Напишите instance MonadError для Either,
   * где в качестве типа ошибки будет String
   */
   type Err[R] = Either[String, R]

   implicit val eitherME: MonadError[Err, String] = new MonadError[Err, String] {

     override def raiseError[A](e: String): Err[A] = Left(e)

     override def handleErrorWith[A](fa: Err[A])(f: String => Err[A]): Err[A] = fa.left.flatMap(f)

     override def handleError[A](fa: Err[A])(f: String => A): Err[A] = fa.left.flatMap(e => pure(f(e)))

     override def ensure[A](fa: Err[A])(e: String)(f: A => Boolean): Err[A] = fa.filterOrElse(f, e)

     override def flatMap[A, B](fa: Err[A])(f: A => Err[B]): Err[B] = fa.flatMap(f)

     override def pure[A](v: A): Err[A] = Right(v)
   }

  def doMathME[F[_], E](start: F[Int])(implicit ec: MonadError[F, E]): F[Int] = {
    val F = implicitly[MonadError[F, E]]
    F.map(start)(_ + 1)
  }
  def doMathMEHandle[F[_], E](start: F[Int])(h: E => Int)(implicit ec: MonadError[F, E]): F[Int] = {
    val F = implicitly[MonadError[F, E]]
    F.handleError(doMathME(start))(h)
  }
}
