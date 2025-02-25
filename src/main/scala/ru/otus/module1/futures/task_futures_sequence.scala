package ru.otus.module1.futures

import ru.otus.module1.futures.HomeworksUtils.TaskSyntax

import scala.concurrent.impl.Promise
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

object task_futures_sequence {

  /**
   * В данном задании Вам предлагается реализовать функцию fullSequence,
   * похожую на Future.sequence, но в отличии от нее,
   * возвращающую все успешные и не успешные результаты.
   * Возвращаемое тип функции - кортеж из двух списков,
   * в левом хранятся результаты успешных выполнений,
   * в правово результаты неуспешных выполнений.
   * Не допускается использование методов объекта Await и мутабельных переменных var
   */
  /**
   * @param futures список асинхронных задач
   * @return асинхронную задачу с кортежом из двух списков
   */
  def fullSequence[A](futures: List[Future[A]])
                     (implicit ex: ExecutionContext): Future[(List[A], List[Throwable])] =
    futures.foldLeft(Future.successful(Vector.empty[A] -> Vector.empty[Throwable])) { (l, r) =>
      r transformWith {
        case Failure(exception) => l map { case (as, throwables) => (as, throwables :+ exception) }
        case Success(value) => l map { case (as, throwables) => (as :+ value, throwables) }
      }
    } map { case (as, throwables) => as.toList -> throwables.toList}

}
