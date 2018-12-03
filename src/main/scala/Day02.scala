import scala.annotation.tailrec
import scala.io.Source

object Day02 {
  def main(args: Array[String]): Unit = {
    println(s"First answer: $solutionOne")
    println(s"Second answer: ${solutionTwo()}")
  }

  def solutionOne: Int = {
    val res = Source.fromResource("day02.txt").getLines.flatMap { boxId =>
      boxId.groupBy(identity)
        .foldLeft(Set.empty[Int]) { (checksum, groups) =>
          groups._2.length match {
            case 2 => checksum + 2
            case 3 => checksum + 3
            case _ => checksum
          }
        }
    }.toSeq.groupBy(identity)

    res(2).length * res(3).length
  }

  @tailrec
  def solutionTwo(index: Int = 0): String = {
    val groups = Source.fromResource("day02.txt").getLines.map { boxId =>
      boxId.take(index) + boxId.drop(index + 1)
    }.toSeq
      .groupBy(identity).filter { group =>
      group match {
        case (_, dupes) if dupes.lengthCompare(2) == 0 => true
        case _ => false
      }
    }

    if (groups.nonEmpty)
      groups.head._1
    else
      solutionTwo(index + 1)
  }
}
