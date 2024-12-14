package se.johan1a.adventofcode2024

import se.johan1a.adventofcode2024
import se.johan1a.adventofcode2024.Utils.*

object Day12:

  private var seen = Set[Pos]()

  def part1(input: Seq[String]): Int =
    val grid = makeGrid(input)
    seen = Set[Pos]()
    allPositions(grid).filterNot(seen.contains).map(pos =>
      val (area, perimeter, nbrPerimeter) = calculate(grid, pos)
      area * nbrPerimeter
    ).sum

  private def calculate(grid: Utils.Grid, start: Vec2): (Int, Set[Pos], Int) =
    var queue = Seq(start)
    var area = 0
    var perimeter = Set[Pos]()
    var nbrPerimeter = 0
    val plantType = get(grid, start)
    while queue.nonEmpty do
      val pos = queue.head
      queue = queue.tail
      if !seen.contains(pos) then
        seen = seen + pos
        area += 1

        Seq(Vec2(0, 1), Vec2(0, -1), Vec2(1, 0), Vec2(-1, 0))
          .map(d => pos + d).foreach(neighbor =>
            if getOpt(grid, neighbor).contains(plantType) then
              queue = queue :+ neighbor
            else if isPerimiter(grid, plantType, neighbor) then
              perimeter = perimeter + neighbor
              nbrPerimeter += 1
          )
    (area, perimeter, nbrPerimeter)

  private def isPerimiter(grid: Grid, plantType: Char, neighbor: Pos) =
    !inRange(grid, neighbor) || get(grid, neighbor) != plantType

  def part2(input: Seq[String]): Int =
    val grid = makeGrid(input)
    seen = Set[Pos]()
    allPositions(grid).filterNot(seen.contains).map(pos =>
      val (area, perimiter, _) = calculate(grid, pos)
      if area > 0 then
        val plantType = get(grid, pos)
        val nbrSides = getNbrSides(grid, perimiter, plantType)

        area * nbrSides
      else
        0
    ).sum

  private def getNbrSides(grid: Grid, perimeter: Set[Pos], plantType: Char) =
    var seen = Set[(Pos, Dir)]()
    var nbrSides = 0
    var first = true
    perimeter
      .toSeq
      .sortBy(p => (p.y, p.x))
      .foreach { start =>
        if !seen.map(_._1).contains(start) then
          var pos = start
          var dir = Left
          if first then
            dir = Right
            first = false
          while !seen.contains((pos, dir)) do
            seen = seen + ((pos, dir))

            val nextPosInFront = pos + dir
            val nextPosToTheRight = getNextToTheRight(grid, pos, dir)
            val nextInFront = perimeter.contains(nextPosInFront)
            val nextRight = perimeter.contains(nextPosToTheRight)
            val rightOf = perimeter.contains(getPosToTheRight(grid, pos, dir))

            (nextInFront, nextRight, rightOf) match
              case (_, true, false) =>
                pos = pos + dir
                dir = turnRight(dir)
                pos = pos + dir
                nbrSides += 1
              case (_, _, true) =>
                nbrSides += 1
                dir = turnRight(dir)
              case (false, false, false) =>
                dir = turnLeft(dir)
                nbrSides += 1
              case (_, false, _) =>
                pos = pos + dir
      }
    nbrSides

  private def getNextToTheRight(grid: Grid, pos: Pos, dir: Dir) =
    val inFront = pos + dir
    val dirToTheRight = turnRight(dir)
    inFront + dirToTheRight

  private def getPosToTheLeft(grid: Grid, pos: Pos, dir: Dir) =
    val dirToTheLeft = turnLeft(dir)
    pos + dirToTheLeft

  private def getPosToTheRight(grid: Grid, pos: Pos, dir: Dir) =
    val dirToTheRight = turnRight(dir)
    pos + dirToTheRight
