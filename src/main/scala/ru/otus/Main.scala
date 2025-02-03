package ru.otus

import ru.otus.module1.{hof, threads, type_system}

object Main {

  def main(args: Array[String]): Unit = {

      println(s"Hello " +
        s"from ${Thread.currentThread().getName}")
      val t0 = new threads.Thread1
      val t1 = new Thread{
        override def run(): Unit = {
          Thread.sleep(1000)
          println(s"Hello " +
            s"from: ${Thread.currentThread().getName}")
        }
      }
//      t1.start()
//      t1.join()
//      t0.start()

    def rates = {
      val t1 = threads.getRatesLocation7
      val t2 = threads.getRatesLocation8


      t1.onComplete{ i1 =>
        t2.onComplete{ i2 =>
          println(s"Sum ${i1 + i2}")
        }
      }

//     val sum: threads.ToyFuture[Int] = for{
//        i1 <- t1
//        i2 <- t2
//      } yield i1 + i2
//
//     sum.onComplete(println)
    }



    threads.printRunningTime(rates)

  }
}
