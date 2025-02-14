trait Show[A]{
  def show(a:A): String
}

object Show {
  def apply[A](using instance: Show[A]): Show[A] = instance
  
  given Show[Int] with {
    def show(a:Int): String = (a*10).toString
  }
  
  given Show[String] with {
    def show(a:String): String = a
  }
  
}

object Main extends  App {
  def printShow[A](a:A)(using show: Show[A]): Unit = {
    println(show.show(a))
  }
  
  printShow(42)
  printShow("42")
}