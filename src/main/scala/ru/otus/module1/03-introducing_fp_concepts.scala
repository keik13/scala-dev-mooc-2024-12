package ru.otus.module1

import scala.annotation.tailrec
import scala.language.postfixOps



/**
 * referential transparency
 */




 // recursion

object recursion {

  /**
   * Реализовать метод вычисления n!
   * n! = 1 * 2 * ... n
   */

  def fact(n: Int): Int = {
    var _n = 1
    var i = 2
    while (i <= n){
      _n *= i
      i += 1
    }
    _n
  }


  def factRec(n: Int): Int = if(n <= 0) 1 else n * factRec(n - 1)


  def factTailRec(n: Int): Int = {
    @tailrec
    def loop(x: Int, accum: Int): Int = {
      if( n <= 0) accum
      else loop(x - 1, x * accum)
    }
    loop(n, 1)
  }




  /**
   * реализовать вычисление N числа Фибоначчи
   * F0 = 0, F1 = 1, Fn = Fn-1 + Fn - 2
   */
  def fib(n: Int): Int = {
    var prev = 0
    var cur = 1
    var i = 1

    if (n == 0) {
      cur = prev
    } else {
      while (i < n) {
        val next = cur + prev
        prev = cur
        cur = next
        i += 1
      }
    }

    cur
  }

  def fibRec(n: Int): Int =
  if(n <= 1) {
    n
  }
  else {
    fibRec(n - 1) + fibRec(n - 2)
  }

  def fibTailRec(n: Int): Int = {
      @tailrec
      def loop(i: Int, prev: Int, cur: Int): Int =
        if( i == 0)
          prev
        else
          loop(i = i-1, prev = cur, cur = prev + cur)

    loop(n, 0, 1)
  }
}



object hof{

  def dumb(string: String): Unit = {
    Thread.sleep(1000)
    println(string)
  }

  // обертки

  def logRunningTime[A, B](f: A => B): A => B = a => {
    val start = System.currentTimeMillis()
    val result: B = f(a)
    val end = System.currentTimeMillis()
    println(s"Running time: ${end - start}")
    result
  }



  // изменение поведения ф-ции


  def isOdd(i: Int): Boolean = i % 2 > 0

  def not[A](f: A => Boolean): A => Boolean = a => !f(a)

  lazy val isEven: Int => Boolean = not(isOdd)



  // изменение самой функции

  def partial[A, B, C](a: A, f: (A, B) => C): B => C =
    b => f(a, b)

  def partial2[A, B, C](a: A, f: (A, B) => C): B => C = f.curried(a)


  def sum(x: Int, y: Int): Int = x + y


  val p: Int => Int = partial(3, sum)
  p(2) // 5
  p(3) // 5



















}






/**
 *  Реализуем тип Option
 */



 object opt {


  class Animal
  class Dog extends Animal
  class Cat extends Animal

  def treat(animal: Animal): Unit = ()
  def treat(animal: Option[Animal]): Unit = ()

  val d: Dog = new Dog
  val dOpt: Option[Dog] = Some(d)
  treat(d)
  treat(dOpt)

  /**
   *
   * Реализовать структуру данных Option, который будет указывать на присутствие либо отсутсвие результата
   */

  // Variance
  // 1. Invariance
  // 2. Covariance
  // 3. Contrvariance

  trait Option[+T]{
    def isEmpty: Boolean = if(this.isInstanceOf[None.type]) true else false

    def get: T = this match {
      case None => throw new Exception("empty Option")
      case Some(v) => v
    }

    def map[B](f: T => B): Option[B] = flatMap(v => Option(f(v)))

    def flatMap[B](f: T => Option[B]): Option[B] = this match {
      case None => None
      case Some(v) => f(v)
    }
  }

  object Option{
    def apply[T](v: T): Option[T] = Some(v)
  }

  val o1: Option[Int] = Option(1)

  val o2: Option[Int] = o1.map(_ + 2)

  case class Some[T](v: T) extends Option[T]
  case object None extends Option[Nothing]

  var o: Option[Animal] = None
  var i: Option[Int] = None







  /**
   *
   * Реализовать метод printIfAny, который будет печатать значение, если оно есть
   */
  def printIfAny[T](v: Option[T]): Unit = v.map(println)

  /**
   *
   * Реализовать метод zip, который будет создавать Option от пары значений из 2-х Option
   */
  def zip[A, B](v1: Option[A], v2: Option[B]): Option[(A, B)] = v1.flatMap(a => v2.map(b => (a, b)))

  /**
   *
   * Реализовать метод filter, который будет возвращать не пустой Option
   * в случае если исходный не пуст и предикат от значения = true
   */
  def filter[T](v: Option[T])(predicate: T => Boolean): Option[T] = v.flatMap(a => if(predicate(a)) Some(a) else None)

 }

 object list {
   /**
    *
    * Реализовать односвязанный иммутабельный список List
    * Список имеет два случая:
    * Nil - пустой список
    * Cons - непустой, содержит первый элемент (голову) и хвост (оставшийся список)
    */


   trait List[+T]{
     def ::[TT >: T](elem: TT): List[TT] = list.::(elem, this)
   }
   case class ::[T](head: T, tail: List[T]) extends List[T]
   case object Nil extends List[Nothing]

   object List{
     def apply[A](v: A*): List[A] = if(v.isEmpty) Nil
     else ::(v.head, apply(v.tail:_*))
   }

   val l1: List[Nothing] = List()
   val l2 = List(1, 2, 3)



    /**
      * Конструктор, позволяющий создать список из N - го числа аргументов
      * Для этого можно воспользоваться *
      *
      * Например вот этот метод принимает некую последовательность аргументов с типом Int и выводит их на печать
      * def printArgs(args: Int*) = args.foreach(println(_))
      */
    def apply[A](v: A*): List[A] = if(v.isEmpty) Nil else ::(v.head, apply(v.tail:_*))
    /**
      *
      * Реализовать метод reverse который позволит заменить порядок элементов в списке на противоположный
      */
    def reverse[T](l: List[T]): List[T] = {
      @tailrec
      def loop(list: List[T], res: List[T]): List[T] = list match {
        case ::(head, tail) => loop(tail, head :: res)
        case Nil => res
      }
      loop(l, list.Nil)
    }
    /**
      *
      * Реализовать метод map для списка который будет применять некую ф-цию к элементам данного списка
      */
    def map[A, B](l: List[A])(f: A => B): List[B] = {
      @tailrec
      def loop(list: List[A], res: List[B]): List[B] = list match {
        case ::(head, tail) => loop(tail, f(head) :: res)
        case Nil => reverse(res)
      }
      loop(l, Nil)
    }

    /**
      *
      * Реализовать метод filter для списка который будет фильтровать список по некому условию
      */
    def filter[T](l: List[T])(p: T => Boolean): List[T] = {
      @tailrec
      def loop(list: List[T], res: List[T]): List[T] = list match {
        case ::(head, tail) if p(head) => loop(tail, head :: res)
        case ::(_, tail) => loop(tail, res)
        case Nil => reverse(res)
      }
      loop(l, Nil)
    }
    /**
      *
      * Написать функцию incList котрая будет принимать список Int и возвращать список,
      * где каждый элемент будет увеличен на 1
      */
    def incList(l: List[Int]): List[Int] = map(l)(_ + 1)

    /**
      *
      * Написать функцию shoutString котрая будет принимать список String и возвращать список,
      * где к каждому элементу будет добавлен префикс в виде '!'
      */
    def shoutString(l: List[String]): List[String] = map(l){a => s"!$a"}
 }