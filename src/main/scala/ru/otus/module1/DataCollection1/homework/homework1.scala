package ru.otus.module1.DataCollection1

class BallsExperiment {
  val urn = List(1,0,1,0,1,0)

  def isFirstBlackSecondWhite(): Boolean = {
    val firstChoice = util.Random.nextInt(urn.size)
    val firstRes = urn(firstChoice) == 1
    val reducedUrn = urn.take(firstChoice) ++ urn.drop(firstChoice + 1)
    val secondChoice = util.Random.nextInt(reducedUrn.size)
    val secondRes = reducedUrn(secondChoice) == 1
    firstRes || secondRes
  }
}

object BallsTest {
  def main(args: Array[String]): Unit = {
    val count = 1000000
    val listOfExperiments: List[BallsExperiment] = List.fill(count)(new BallsExperiment)
    val countOfExperiments = listOfExperiments.map(_.isFirstBlackSecondWhite())
    val countOfPositiveExperiments: Float = countOfExperiments.count(_ == true)
    println(countOfPositiveExperiments / count)
  }
}