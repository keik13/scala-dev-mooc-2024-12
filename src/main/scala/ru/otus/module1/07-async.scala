package ru.otus.module1

import ru.otus.module1.utils.NameableThreads

import java.io.File
import java.util.concurrent.{Executor, ExecutorService, Executors}
import scala.collection.mutable
import scala.concurrent.ExecutionContext.global
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor, Future, Promise}
import scala.io.{BufferedSource, Source}
import scala.language.{existentials, postfixOps}
import scala.util.{Failure, Success, Try}

object threads {


  // Thread

  class Thread1 extends Thread{
    override def run(): Unit = {
      println(s"Hello from: " +
        s"${Thread.currentThread().getName}")
    }
  }

  def printRunningTime(f: => Unit): Unit = {
    val start = System.currentTimeMillis()
    f
    val end = System.currentTimeMillis()
    println(s"Running time: ${end - start}")
  }

  def async(f: => Unit): Thread = new Thread{
    override def run(): Unit = f
  }

  def getRatesLocation1 = {
    Thread.sleep(1000)
    println("GetRatesLocation1")
  }
  def getRatesLocation2 = {
    Thread.sleep(2000)
    println("GetRatesLocation2")
  }

  def getRatesLocation3 = async{
    Thread.sleep(1000)
    println("GetRatesLocation3")
  }
  def getRatesLocation4 = async{
    Thread.sleep(2000)
    println("GetRatesLocation4")
  }

  def async2[A](f: => A): A = {
    var v: A = null.asInstanceOf[A]
    val t = new Thread{
      override def run(): Unit = v = f
    }
    t.start()
    t.join()
    v
  }

  def getRatesLocation5: Int = async2{
    Thread.sleep(1000)
    println("GetRatesLocation5")
    10
  }
  def getRatesLocation6: Int = async2{
    Thread.sleep(2000)
    println("GetRatesLocation6")
    20
  }

  class ToyFuture[T] private(v: () => T){

    private var r: T = null.asInstanceOf[T]
    private var isCompleted: Boolean = false
    private val q = mutable.Queue[T => _]()

    def onComplete[U](f: T => U): Unit = {
      if(isCompleted) f(r)
      else q.enqueue(f)
    }

    def flatMap[B](f: T => ToyFuture[B]): ToyFuture[B] = ???
    def map[B](f: T => B): ToyFuture[B] = ???

    def start(executor: Executor) = {
      val t = new Runnable {
        override def run(): Unit = {
          val result = v()
          r = result
          isCompleted = true
          while (q.nonEmpty){
            q.dequeue()(result)
          }
        }
      }
      executor.execute(t)
    }
  }

  object ToyFuture{
    def apply[T](f: => T)(implicit executor: Executor): ToyFuture[T] = {
      val tf = new ToyFuture(() => f)
      tf.start(executor)
      tf
    }
  }

  implicit val ec = executor.pool3
  def getRatesLocation7: ToyFuture[Int] = ToyFuture{
    Thread.sleep(1000)
    println("GetRatesLocation5")
    10
  }
  def getRatesLocation8: ToyFuture[Int] = ToyFuture{
    Thread.sleep(2000)
    println("GetRatesLocation6")
    20
  }













}










object executor {
      val pool1: ExecutorService =
        Executors.newFixedThreadPool(2, NameableThreads("fixed-pool-1"))
      val pool2: ExecutorService =
        Executors.newCachedThreadPool(NameableThreads("cached-pool-2"))
      val pool3: ExecutorService =
        Executors.newWorkStealingPool(4)
      val pool4: ExecutorService =
        Executors.newSingleThreadExecutor(NameableThreads("singleThread-pool-4"))
}