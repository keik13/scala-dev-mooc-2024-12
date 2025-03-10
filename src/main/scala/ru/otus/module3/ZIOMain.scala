package ru.otus.module3

import zio._


object ZIOMain {

  def main(args: Array[String]): Unit = {
    val z1 = ZIO.attempt(println("Hello"))
    val environment: ZEnvironment[Console & Clock] =
      ZEnvironment[Console, Clock](Console.ConsoleLive, Clock.ClockLive)



    Unsafe.unsafe { implicit unsafe =>
      zio.Runtime.default.unsafe.run(
        zioConcurrency.printEffectRunningTime(
          zioConcurrency.p4)
          .provideEnvironment(environment)
      )
    }
  }

}

object ZIOMain2 extends ZIOAppDefault{
  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = ???
}


