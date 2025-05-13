package ru.otus.module4.catsstreamingjson.homework

import cats.effect.kernel.{Ref, Resource}
import cats.effect.{IO, IOApp}
import cats.implicits.toTraverseOps
import com.comcast.ip4s.IpLiteralSyntax
import org.http4s.HttpRoutes
import org.http4s.Method.GET
import org.http4s.dsl.impl.IntVar
import org.http4s.dsl.io._
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.server.Server
import ru.otus.module4.catsstreamingjson.homework.Client.{counter, slow}
import fs2.{Chunk, Stream}

import scala.concurrent.duration.{DurationInt, FiniteDuration}

object Server {

  def routes(c: Ref[IO, Int]): HttpRoutes[IO] = HttpRoutes.of {
    case GET -> Root / "counter" => counterProgram(c).flatMap(c => Ok(c.toString))
    case GET -> Root / "slow" / IntVar(chunk) / IntVar(total) / IntVar(time) => Ok(dataStream(total, chunk, time.seconds))
  }
  def counterProgram(c: Ref[IO, Int]): IO[Int] = for {
    _ <- c.update(_ + 1)
    value <- c.get
  }  yield value

  def dataStream(maxBytes: Int, chunkSize: Int, delay: FiniteDuration): Stream[IO, Chunk[Byte]] = Stream.iterate(0)(identity).covary[IO]
    .map(_.toByte)
    .take(maxBytes)
    .chunkN(chunkSize)
    .metered(delay)
    .evalTap { chunk =>
      IO(println(s"Чанк: ${chunk.size} байт | Данные: ${chunk.toList.map(_ & 0xFF).mkString(", ")}"))
    }

  val server: Resource[IO, Server] = for {
    c <- Resource.eval(Ref.of[IO, Int](0))
    s <- EmberServerBuilder
      .default[IO]
      .withPort(port"8080")
      .withHost(ipv4"0.0.0.0")
      .withHttpApp(routes(c).orNotFound).build
  } yield s

}

object Client {
  val builder = EmberClientBuilder.default[IO].build

  val counter = builder.use(
    client => client.expect[String]("http://localhost:8080/counter")
  )

  val slow = builder.use(
    client => client.expect[String]("http://localhost:8080/slow/23/43/4")
  )
}

object CounterTest extends IOApp.Simple {
  def run: IO[Unit] = {
    for {
      _ <- Server.server.use(_ => (1 to 3).toList.traverse(_ => counter).flatMap(IO.println))
    } yield()
  }
}

object SlowTest extends IOApp.Simple {
  def run: IO[Unit] = {
    for {
      _ <- Server.server.use(_ => slow)
    } yield()
  }
}
