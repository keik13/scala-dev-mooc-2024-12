package ru.otus.module1

object functions {


  /**
   * Функции
   */



  /**
   * Реализовать метод  sum, которая будет суммировать 2 целых числа и выдавать результат
   */

    def sum(x: Int, y: Int): Int = x + y


    val r1: Int = sum(2, 3) // 5


    val sum2: (Int, Int) => Int = (v1: Int, v2: Int) => v1 + v2


    val r2 = sum2(2, 3) // 5

    val sum3: (Int, Int) => Int = sum _

     sum3(2, 3) // 5

  // Partial function

  val divide0 : (Int, Int) => Int =
    (a, b) => a / b

  val divide: PartialFunction[(Int, Int), Int] = {
    case x if x._2 != 0 => x._1 / x._2
  }

  divide.isDefinedAt(1, 0) // false


   val ll = List((4, 2), (5, 0), (6, 2))
   val ll2: List[Int] = ll.collect(divide)



  // SAM Single Abstract Method

  trait Printer{
    def print(str: String): Unit
  }

  val p: Printer = str => println(str)



  /**
   *  Задание 1. Написать ф-цию метод isEven, которая будет вычислять является ли число четным
   */
  def isEven(i: Int): Boolean = i % 2 == 0

  /**
   * Задание 2. Написать ф-цию метод isOdd, которая будет вычислять является ли число нечетным
   */
  def isOdd(i: Int): Boolean = !isEven(i)

  /**
   * Задание 3. Написать ф-цию метод filterEven, которая получает на вход массив чисел и возвращает массив тех из них,
   * которые являются четными
   */
  def filterEven(array: Array[Int]): Array[Int] = array.filter(isEven)

  /**
   * Задание 4. Написать ф-цию метод filterOdd, которая получает на вход массив чисел и возвращает массив тех из них,
   * которые являются нечетными
   */
  def filterOdd(array: Array[Int]): Array[Int] = array.filter(isOdd)


  /**
   * return statement
   *
   */

  // Currying

  // (Int, Int) => Int Int => Int => Int

  val c1: Int => Int => Int = sum2.curried


  val rr2: Int => Int = c1(2)

  rr2(3)
}