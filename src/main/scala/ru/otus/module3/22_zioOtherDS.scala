package ru.otus.module3

import zio.{Promise, Schedule, UIO, ZIO, durationInt}

import java.util.concurrent.atomic.AtomicReference
import scala.language.postfixOps

object zioDS {

  object schedule {

    val eff = ZIO.attempt(println("hello"))

    /** 1. Написать эффект, который будет выводить в консоль Hello 5 раз
     */

    val schedule1 = Schedule.recurs(5)
    val eff1 = eff.repeat(schedule1)



    /** 2. Написать эффект, который будет выводить в консоль Hello 5 раз, раз в секунду
     */

    val schedule2 = Schedule.fixed(1 seconds)
    val eff2 = eff.repeat(schedule1 && schedule2)


    /** Написать эффект, который будет генерить произвольное число от 0 до 10,
     * и повторяться пока число не будет равным 0
     */

    val schedule3 = Schedule.recurWhile[Int]( _ > 0)
    val random = zio.Random.nextIntBetween(0, 11)
    val eff3 = random.repeat(schedule3)




    /** Написать планировщик, который будет выполняться каждую пятницу 12 часов дня
     */

    val schedule4 = Schedule.dayOfWeek(5) && Schedule.hourOfDay(12)


  }

  object ref {

    /**
     * Счетчик
     *
     */

    var counter: Int = 0

    val updateCounter = ZIO.foreachPar((1 to 3).toList)(_ => ZIO.succeed(counter += 1))
      .as(counter)


    trait Ref[A] {
      def modify[B](f: A => (B, A)): UIO[B]

      def get: UIO[A] = modify(a => (a, a))

      def set(a: A): UIO[Unit] = modify(_ => ((), a))

      def update[B](f: A => A): UIO[Unit] =
        modify(a => ((), f(a)))
    }

    object Ref{
      def make[A](a: A): UIO[Ref[A]] = ZIO.succeed{
        new Ref[A]{
          val atomic = new AtomicReference(a)
          override def modify[B](f: A => (B, A)): UIO[B] = ZIO.succeed{
            var cond = true
            var r: B = null.asInstanceOf[B]
            while (cond){
              val current = atomic.get
              val (b, a) = f(current)
              r = b
              cond = !atomic.compareAndSet(current, a)
            }
            r
          }
        }
      }
    }

    val updateCounterRef = for{
      counter <- Ref.make(0)
      _ <- ZIO.foreachPar((1 to 3).toList)(_ => counter.update(_ + 1))
      res <- counter.get
    } yield res

  }
}
