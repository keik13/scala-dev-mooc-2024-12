package ru.otus.module2

object homework_hkt_implicits{

  trait Bindable[F[_], A]{
    def map[B](f: A => B): F[B]
    def flatMap[B](f: A => F[B]): F[B]
  }

  def tuplef[F[_], A, B](fa: Bindable[F, A], fb: Bindable[F, B]): F[(A, B)] =
    fa.flatMap(a => fb.map(b => (a, b)))

  implicit def optBindable[A](opt: Option[A]): Bindable[Option, A] = new Bindable[Option, A] {
    override def map[B](f: A => B): Option[B] = opt.map(f)
    override def flatMap[B](f: A => Option[B]): Option[B] = opt.flatMap(f)
  }
  //и тд для всех нужных нам тайп конструкторов

  // вариант 2
  def tuplef[F[_], A, B](fa: F[A], fb: F[B])(implicit convA: F[A] => Bindable[F, A], convB: F[B] => Bindable[F, B]): F[(A, B)] =
    convA(fa).flatMap(a => convB(fb).map(b => (a, b)))

  // вариант 3
  trait Bindable2[F[_]]{
    def map[A, B](b: F[A])(f: A => B): F[B]
    def flatMap[A, B](b: F[A])(f: A => F[B]): F[B]
  }
  def tuplef2[F[_], A, B](fa: F[A], fb: F[B])(implicit convA: Bindable2[F]): F[(A, B)] =
    convA.flatMap(fa)(a => convA.map(fb)(b => (a, b)))

}