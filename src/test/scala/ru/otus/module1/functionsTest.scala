package ru.otus.module1

import org.scalatest.funsuite.AnyFunSuiteLike

class functionsTest extends AnyFunSuiteLike {

  test("testFilterOdd") {
    assert(functions.filterOdd(Array(1, 4, 5)) sameElements Array(1, 5))
  }

  test("testIsEven") {
    assert(functions.isEven(42))
    assert(!functions.isEven(127))
  }

  test("testFilterEven") {
    assert(functions.filterEven(Array(1, 4, 5)) sameElements Array(4))
  }

  test("testIsOdd") {
    assert(!functions.isOdd(42))
    assert(functions.isOdd(127))
  }

}
