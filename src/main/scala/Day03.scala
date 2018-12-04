import scala.annotation.tailrec
import scala.io.Source

object Day03 {
  def main(args: Array[String]): Unit = {
    println(s"First answer: $solutionOne")
    println(s"Second answer: ${solutionTwo()}")
  }

  private def processInput: Seq[Claim] = {
    Source.fromResource("day03.txt").getLines.map { claim =>

      val regex = """#(\d+)\s@\s(\d+),(\d+):\s(\d+)x(\d+)""".r
      val regex(claimIdStr, leftEdgeStr, topEdgeStr, widthStr, heightStr) = claim
      val claimId = claimIdStr.toInt
      val leftEdge = leftEdgeStr.toInt
      val topEdge = topEdgeStr.toInt
      val width = widthStr.toInt
      val height = heightStr.toInt

      Claim(claimId, Coordinates(leftEdge, topEdge), Coordinates(leftEdge + width, topEdge), Coordinates(leftEdge, topEdge + height), Coordinates(leftEdge + width, topEdge + height))
    }.toList
  }

  private def processAllClaims(claims: Seq[Claim]): Seq[Seq[Int]] = {
    val width = claims.map(_.bottomRight.x).max
    val height = claims.map(_.bottomRight.y).max

    claims.foldLeft(Seq.fill(width + 1, height + 1)(0)) { (fabric, claim) =>
      for (x <- 0 to width) yield {
        for (y <- 0 to height) yield {
          if (x >= claim.topLeft.x && y >= claim.topLeft.y &&
            x < claim.topRight.x && y >= claim.topRight.y &&
            x >= claim.bottomLeft.x && y < claim.bottomLeft.y &&
            x < claim.bottomRight.x && y < claim.bottomRight.y) {
            fabric(x)(y) match {
              case 0 => claim.claimId
              case _ => -1
            }
          }
          else fabric(x)(y)
        }
      }
    }
  }

  def solutionOne: Int = {
    processAllClaims(processInput).flatten.count(_ == -1)
  }

  @tailrec
  def solutionTwo(claims: Seq[Claim] = Seq.empty, allClaims: Seq[Seq[Int]] = Seq(Seq.empty)): Int = {
    if (claims.isEmpty) {
      val claims = processInput
      val allClaims = processAllClaims(claims)

      solutionTwo(claims, allClaims)
    } else {
      val claim = claims.head

      val res = for (x <- claim.topLeft.x until claim.topRight.x) yield {
        for (y <- claim.topLeft.y until claim.bottomLeft.y) yield {
          allClaims(x)(y) match {
            case claim.claimId => true
            case _ => false
          }
        }
      }

      if (res.flatten.fold(true)(_ & _))
        claim.claimId
      else
        solutionTwo(claims.tail, allClaims)
    }
  }
}

case class Claim(claimId: Int, topLeft: Coordinates, topRight: Coordinates, bottomLeft: Coordinates, bottomRight: Coordinates)

case class Coordinates(x: Int, y: Int)