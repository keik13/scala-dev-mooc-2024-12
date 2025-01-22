package ru.otus.module1

import org.scalatest.funsuite.AnyFunSuiteLike

class recursionTest extends AnyFunSuiteLike {

  test("testFib") {
    assert(recursion.fib(0) == 0 && recursion.fibRec(0) == 0 && recursion.fibTailRec(0) == 0)
    assert(recursion.fib(1) == 1 && recursion.fibRec(1) == 1 && recursion.fibTailRec(1) == 1)
    assert(recursion.fib(2) == 1 && recursion.fibRec(2) == 1 && recursion.fibTailRec(2) == 1)
    assert(recursion.fib(3) == 2 && recursion.fibRec(3) == 2 && recursion.fibTailRec(3) == 2)
    assert(recursion.fib(4) == 3 && recursion.fibRec(4) == 3 && recursion.fibTailRec(4) == 3)
    assert(recursion.fib(10) == 55 && recursion.fibRec(10) == 55 && recursion.fibTailRec(10) == 55)
  }

}
