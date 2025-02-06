package ru.otus.module2

import ru.otus.module2.higher_kinded_types.Bindable

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
}