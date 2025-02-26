package ru.otus.module2.catsHomework

import org.scalatest.funsuite.AnyFunSuiteLike

import scala.util.{Failure, Try}

class packageTest extends AnyFunSuiteLike {

  test("testDoMath") {
    val b1: Tree[Int] = Branch(Leaf(1), Leaf(2))
    assert(doMath(b1) == Branch(Leaf(2), Leaf(3)))

    val b2: Tree[Int] = Branch(Leaf(4), Branch(Leaf(1), Leaf(2)))
    assert(doMath(b2) == Branch(Leaf(5), Branch(Leaf(2), Leaf(3))))

    val l1: Tree[Int] = Leaf(3)
    assert(doMath(l1) == Leaf(4))
  }

  test("testDoMathME") {
    val t1 = Try(3)
    assert(doMathME(t1) == Try(4))

    val t2: Either[String, Int] = Right(3)
    assert(doMathME(t2) == Right(4))
  }

  test("doMathMEHandle") {
    val t1: Try[Int] = Failure(new Exception("bug"))
    assert(doMathMEHandle(t1){_: Throwable => -1} == Try(-1))

    val t2: Either[String, Int] = Left("bug")
    assert(doMathMEHandle(t2){_: String => -1} == Right(-1))
  }

}
