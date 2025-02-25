package ru.otus.module1

import org.scalatest.funsuite.AnyFunSuiteLike

class optTest extends AnyFunSuiteLike {

  test("testZip") {
    val res = opt.zip(opt.Option(42), opt.Option("raison d'etre"))
    assert(res == opt.Option(42 -> "raison d'etre"))
  }

  test("testPrintIfAny") {
    val stream = new java.io.ByteArrayOutputStream()
    Console.withOut(stream) {
      opt.printIfAny(opt.Option(33))
    }
    assert(stream.toString == "33\n")

    stream.reset()
    Console.withOut(stream) {
      opt.printIfAny(opt.None)
    }
    assert(stream.toString == "")
    stream.close()
  }

  test("testFilter") {
    val res = opt.filter(opt.Option(42))(_ < 100)
    assert(res == opt.Option(42))

    val res2 = opt.filter(opt.Option(42))(_ > 100)
    assert(res2 == opt.None)
  }

}
