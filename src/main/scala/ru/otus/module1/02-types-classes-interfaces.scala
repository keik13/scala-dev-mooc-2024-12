package ru.otus.module1

import java.io.{Closeable, File}
import scala.io.{BufferedSource, Source}
import scala.util.{Try, Using}



object type_system {

  /**
   * Scala type system
   *
   */



  def absurd(v: Nothing) = ???


  // Generics


//  lazy val file: File = ???
//  lazy val source: BufferedSource = Source.fromFile(file)
//
//  lazy val lines: List[String] = try{
//    source.getLines().toList
//  } finally {
//    source.close()
//  }

  //lines.foreach(println(_))

  def ensureClose[S, R](source: S)(release: S => Any)(f: S => R): R = {
    try{
      f(source)
    } finally {
     release(source)
    }
  }

  def ensureClose2[S, R](acquire: => S)(release: S => Any)(f: S => R): R = ???

  def ensureCloseFixed[R]: (BufferedSource => R) => R =
    ensureClose2(Source.fromFile(""))(_.close())

//  ensureClose(Source.fromFile(file))(s => s.close()){ s =>
//    val l = s.getLines().toList
//    l.foreach(println)
//  }



















  // ограничения связанные с дженериками


  /**
   *
   * class
   *
   * конструкторы / поля / методы / компаньоны
   */


   class User private(val email: String, val password: String){


    def getMail: String = email
    def getPassword: String = password

    def this(email: String) = this(email, "123456")
   }

   object User{
     def from(email: String, password: String): User = new User(email, password)
     def from(email: String): User = new User(email, "12345")
   }

   val user: User = User.from("foo@gmail.com", "12345")
   val user2: User = User.from("foo@gmail.com")
   val user3: User = new User("foo@gmail.com")






  case class User2(email: String = "foo@gmail.com", password: String = "123456")

  val user4 = User2(password = "122345t")

  val user5 = user4.copy(password = "9087")



  /**
   * Задание 1: Создать класс "Прямоугольник"(Rectangle),
   * мы должны иметь возможность создавать прямоугольник с заданной
   * длиной(length) и шириной(width), а также вычислять его периметр и площадь
   *
   */


  /**
   * object
   *
   * 1. Паттерн одиночка
   * 2. Ленивая инициализация
   * 3. Могут быть компаньоны
   */


  /**
   * case class
   *
   */



    // создать case класс кредитная карта с двумя полями номер и cvc





  /**
   * case object
   *
   * Используются для создания перечислений или же в качестве сообщений для Акторов
   */



  /**
   * trait
   *
   */



    sealed trait UserService{
      def get(id: String): User
      def insert(user: User): Unit

    }

    trait Updatable{
      def update(user: User): User
    }

    class UserServiceImpl extends UserService with Updatable {
      override def get(id: String): User = ???

      override def insert(user: User): Unit = ???

      override def update(user: User): User = ???
    }

    val userService: UserService with Updatable = new UserService with Updatable {
      override def get(id: String): User = ???

      override def insert(user: User): Unit = ???

      override def update(user: User): User = ???
    }


















  class A {
    def foo() = "A"
  }

  trait B extends A {
    override def foo() = "B" + super.foo()
  }

  trait C extends B {
    override def foo() = "C" + super.foo()
  }

  trait D extends A {
    override def foo() = "D" + super.foo()
  }

  trait E extends C {
    override def foo(): String = "E" + super.foo()
  }


  // CBDA
  // A -> D -> B -> C
  val v = new A with D with C with B



  // A -> B -> C -> E -> D
  // DECBA
  val v1 = new A with E with D with C with B


  /**
   * Value classes и Universal traits
   */


}