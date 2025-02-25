package ru.otus.module1

import org.scalatest.funsuite.AnyFunSuiteLike

class listTest extends AnyFunSuiteLike {

  test("test::") {
    assert(list.::(1, list.List(4,5)) == list.List(1,4,5))
  }

  test("testApply") {
    assert(list.apply(1,6,8) == list.List(1,6,8))
  }

  test("testIncList") {
    assert(list.incList(list.List(1, 2, 3)) == list.List(2,3,4))
  }

  test("testReverse") {
    assert(list.reverse(list.List(1, 2, 3)) == list.List(3,2,1))
  }

  test("testFilter") {
    assert(list.filter(list.List(1, 2, 3))(_ != 2) == list.List(1,3))
  }

  test("testMap") {
    assert(list.map(list.List(1, 2, 3))(_ + 1) == list.List(2,3,4))
  }

  test("testShoutString") {
    assert(list.shoutString(list.List("1", "2", "3")) == list.List("!1", "!2", "!3"))
  }

}
