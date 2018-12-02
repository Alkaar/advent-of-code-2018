import scala.annotation.tailrec
import scala.io.Source

object Day01 {
  def main(args: Array[String]): Unit = {
    println(s"First answer: $solutionOne")
    println(s"Second answer: ${solutionTwo()}")
  }

  def solutionOne: Int = {
    Source.fromResource("day01.txt").getLines.map { freqChange =>
      freqChange(0) match {
        case '+' => freqChange.drop(1).toInt
        case '-' => freqChange.drop(1).toInt * -1
      }
    }.sum
  }

  @tailrec
  def solutionTwo(stream: Stream[Int] = Stream.empty, acc: Int = 0, set: Set[Int] = Set.empty): Int = {
    if (stream.isEmpty) {
      val freqChangeList = Source.fromResource("day01.txt").getLines.map { freqChange =>
        freqChange(0) match {
          case '+' => freqChange.drop(1).toInt
          case '-' => freqChange.drop(1).toInt * -1
        }
      }.toSeq

      val newStream = Stream.continually(freqChangeList.toStream).flatten

      solutionTwo(newStream, newStream.head + acc)
    } else {
      if (set.contains(acc)) {
        acc
      } else {
        solutionTwo(stream.tail, stream.tail.head + acc, set + acc)
      }
    }
  }
}