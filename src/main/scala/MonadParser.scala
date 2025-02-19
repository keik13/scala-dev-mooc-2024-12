class MonadParser[T, Src](private  val p: Src => (T, Src)):
  def flatMap[M](f: T => MonadParser[M, Src]): MonadParser[M, Src] =
    MonadParser{ src =>
      val (word, rest) = p(src)
      val mn = f(word)
      val res = mn.p(rest)
      res
    }

  def map[M](f: T=>M): MonadParser[M, Src] =
    MonadParser { src =>
      val (word, rest) = p(src)
      (f(word), rest)
    }

  def parse(src: Src): T = p(src)._1


object MonadParser:
  def apply[T, Src](f: Src => (T, Src)) = new MonadParser[T, Src](f)


class ParserWithGivenParam(using splitter: String):
  def StringField: MonadParser[String, String] = MonadParser[String, String] {
    str =>
      val idx = str.indexOf(";")
      if (idx > -1)
        (str.substring(0, idx), str.substring(idx + 1))
      else
        (str, "")
  }

  def IntField: MonadParser[Int, String] = StringField.map(_.toInt)

  def BooleanField: MonadParser[Boolean, String] = StringField.map(_.toBoolean)

  val parser = for
    year <- IntField
    mark <- StringField
    model <- StringField
    canDrive <- BooleanField
  yield Car(year, mark, model, canDrive)

  def result(str: String): Array[Car] = str.split(splitter).map(parser.parse)

object TestExecutor:
  @main def go(): Unit = 
    val str = "1997;Ford;Passat;true\n1901;Ford;T;false"
    given String = "\n"
    val parser = ParserWithGivenParam()
    println(parser.result(str).mkString(" "))

case class Car(year: Int, mark: String, model: String, canDrive: Boolean)