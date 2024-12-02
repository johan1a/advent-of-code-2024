package se.johan1a.adventofcode2024

import scala.io.Source

object TestInputUtil {

  def getInput(path: String): Seq[String] = {
    Source.fromResource(path).getLines.toSeq
  }

}
