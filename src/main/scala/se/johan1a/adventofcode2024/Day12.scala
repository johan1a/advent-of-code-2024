package se.johan1a.adventofcode2024

import se.johan1a.adventofcode2024
import se.johan1a.adventofcode2024.Utils.*

object Day12:

  private var seen = Set[Pos]()

  def part1(input: Seq[String]): Int =
    val grid = makeGrid(input)
    seen = Set[Pos]()
    allPositions(grid).filterNot(seen.contains).map(pos =>
      val (area, perimiter, nbrPerimiter, _) = calculate(grid, pos)
      area * nbrPerimiter
    ).sum

  private def calculate(grid: Utils.Grid, start: Vec2): (Int, Set[Pos], Int, Set[Pos]) =
    var queue = Seq(start)
    var area = 0
    var perimiter = Set[Pos]()
    var nbrPerimiter = 0
    val plantType = get(grid, start)
    var localGroup = Set[Pos]()
    while queue.nonEmpty do
      val pos = queue.head
      queue = queue.tail
      if !seen.contains(pos) then
        seen = seen + pos
        localGroup = localGroup + pos
        area += 1

        val below = Vec2(0, 1) + pos
        val above = Vec2(0, -1) + pos
        val rightOf = Vec2(1, 0) + pos
        val leftOf = Vec2(-1, 0) + pos

        val neighbors = Seq(below, above, rightOf, leftOf)
        neighbors.foreach(neighbor =>
          if getOpt(grid, neighbor).contains(plantType) then
            queue = queue :+ neighbor
          else if isPerimiter(grid, plantType, neighbor) then
            perimiter = perimiter + neighbor
            nbrPerimiter += 1
        )
//        if isPerimiter(grid, plantType, leftOf) && isPerimiter(grid, plantType, above) then
//          perimiter = perimiter + (pos + Vec2(-1, -1))
//        if isPerimiter(grid, plantType, leftOf) && isPerimiter(grid, plantType, below) then
//          perimiter = perimiter + (pos + Vec2(-1, 1))
//        if isPerimiter(grid, plantType, rightOf) && isPerimiter(grid, plantType, above) then
//          perimiter = perimiter + (pos + Vec2(1, -1))
//        if isPerimiter(grid, plantType, rightOf) && isPerimiter(grid, plantType, below) then
//          perimiter = perimiter + (pos + Vec2(1, 1))

    (area, perimiter, nbrPerimiter, localGroup)

  private def isPerimiter(grid: Grid, plantType: Char, neighbor: Pos) =
    !inRange(grid, neighbor) || get(grid, neighbor) != plantType

  def part2(input: Seq[String]): Int =
    val grid = makeGrid(input)
    seen = Set[Pos]()
    allPositions(grid).filterNot(seen.contains).map(pos =>
      // also for each hole, go around it inside. keep track of visited holes
      // add outline to hole nbr sides
      val (area, perimiter, _, group) = calculate(grid, pos)
      if area > 0 then

        val topLeft = group.toSeq.minBy(p => (p.y, p.x))
        val plantType = get(grid, pos)
        val nbrSides = getNbrSides(grid, perimiter, plantType)

        println(s"${get(grid, pos)}, area $area nbrSides $nbrSides")
        area * nbrSides
      else
        0
    ).sum

  private def getNbrSides(grid: Grid, perimiter: Set[Pos], plantType: Char) =
    var seen = Set[(Pos, Dir)]()
    var nbrSides = 0
    var first = true
    perimiter
      .toSeq
      .sortBy(p => (p.y, p.x))
      .foreach { start =>
        if !seen.map(_._1).contains(start) then
          println(s"start $plantType $start")
          var pos = start
          var dir = Left
          if first then
            dir = Right
            first = false
          while !seen.contains((pos, dir)) do
            seen = seen + ((pos, dir))

            assert(getOpt(grid, pos).forall(t => t != plantType))

            println(s"$pos $dir $nbrSides")
            Utils.printGrid(grid, pos, dir, 80, 15)
            println()

            val nextPosInFront = pos + dir
            val nextPosToTheLeft = getNextToTheLeft(grid, pos, dir)
            val nextPosToTheRight = getNextToTheRight(grid, pos, dir)
            val nextLeft = perimiter.contains(nextPosToTheLeft)
            val nextInFront = perimiter.contains(nextPosInFront)
            val nextRight = perimiter.contains(nextPosToTheRight)
            val leftOf = perimiter.contains(getPosToTheLeft(grid, pos, dir))
            val rightOf = perimiter.contains(getPosToTheRight(grid, pos, dir))

            (leftOf, nextLeft, nextInFront, nextRight, rightOf) match
              case (_, _, false, true, false) if dir == Right =>
                pos = pos + dir
                dir = turnRight(dir)
                pos = pos + dir
                nbrSides += 1
              case (_, false, false, false, false) if dir == Right =>
                dir = turnLeft(dir)
                nbrSides += 1
              case (false, _, false, false, true) if dir == Right =>
                dir = turnRight(dir)
                nbrSides += 1
              case (_, _, false, true, false) if dir == Left =>
                pos = pos + dir
                dir = turnRight(dir)
                pos = pos + dir
                nbrSides += 1
              case (_, false, false, true, false) if dir == Down =>
                pos = pos + dir
                dir = turnRight(dir)
                pos = pos + dir
                nbrSides += 1
              case (true, false, false, _, false) if dir == Down =>
                dir = turnLeft(dir)
                nbrSides += 1
              case (_, false, false, true, false) if dir == Down =>
                pos = pos + dir
                dir = turnRight(dir)
                pos = pos + dir
                nbrSides += 1
              case (_, false, false, false, true) if dir == Right =>
                dir = turnRight(dir)
                nbrSides += 1
              case (_, false, false, true, false) if dir == Up =>
                pos = pos + dir
                dir = turnRight(dir)
                pos = pos + dir
                nbrSides += 1
              case (_, true, false, false, false) if dir == Up =>
                dir = turnLeft(dir)
                nbrSides += 1
              case (true, false, false, false, false) =>
                dir = turnLeft(dir)
                nbrSides += 1
              case (_, false, false, false, false) if dir == Down =>
                dir = turnLeft(dir)
                nbrSides += 1
              case (false, false, false, false, true) =>
                dir = turnRight(dir)
                nbrSides += 1
              case (_, false, false, false, _) if dir == Down =>
                dir = turnLeft(dir)
                nbrSides += 1
              case (false, true, false, false, false) =>
                dir = turnLeft(dir)
                nbrSides += 1
              case (_, _, false, false, false) if dir == Down =>
                dir = turnLeft(dir)
                nbrSides += 1
              case (false, false, true, false, false) =>
                pos = pos + dir
              case (_, false, true, false, false) if dir == Down =>
                pos = pos + dir
              case (_, false, true, true, true) if dir == Down =>
                pos = pos + dir
              case (_, false, true, _, true) if dir == Down =>
                nbrSides += 1
                dir = turnRight(dir)
              case (false, false, true, true, true) =>
                pos = pos + dir
              case (false, false, false, false, false) =>
                nbrSides += 1
                dir = turnLeft(dir)
              case (false, false, true, true, false) if dir == Right =>
                pos = pos + dir
                dir = turnRight(dir)
                pos = pos + dir
                nbrSides += 1
              case (false, false, true, _, _) if dir == Right =>
                pos = pos + dir
              case (false, _, false, false, true) if dir == Left =>
                nbrSides += 1
                dir = turnRight(dir)
              case (false, false, true, _, true) if dir == Up =>
                pos = pos + dir
              case (false, true, true, false, false) =>
                pos = pos + dir
              case (true, false, true, false, false) =>
                pos = pos + dir
              case (true, false, true, true, false) =>
                pos = pos + dir
                dir = turnRight(dir)
                pos = pos + dir
                nbrSides += 1
              case (_, _, true, false, false) =>
                pos = pos + dir
              case (false, false, true, true, false) =>
                pos = pos + dir
                dir = turnRight(dir)
                pos = pos + dir
                nbrSides += 1
              case (false, true, false, true, false) =>
                pos = pos + dir
                dir = turnRight(dir)
                pos = pos + dir
                nbrSides += 1
              case (false, true, true, true, false) =>
                pos = pos + dir
                dir = turnRight(dir)
                pos = pos + dir
                nbrSides += 1
              case (true, true, false, false, false) =>
                dir = turnLeft(dir)
                nbrSides += 1
              case (true, true, false, true, false) =>
                pos = pos + dir
                dir = turnRight(dir)
                pos = pos + dir
                nbrSides += 1
              case (true, true, true, true, false) =>
                pos = pos + dir
                dir = turnRight(dir)
                pos = pos + dir
                nbrSides += 1
              case _ => ???

        println(s"$plantType - nbrSides: $nbrSides")
      }
    println(s"$plantType nbrSides: $nbrSides")
    nbrSides

  private def getSame(grid: Grid, originalPlantType: Char, pos: Vec2) =
    getOpt(grid, pos) match
      case Some(plantType) if plantType == originalPlantType => Some(plantType)
      case _                                                 => None

  private def getNextToTheLeft(grid: Grid, pos: Pos, dir: Dir) =
    val inFront = pos + dir
    val dirToTheLeft = turnLeft(dir)
    inFront + dirToTheLeft

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

//EEEEEEEE
//EEEEEEEE
//EExEXXXX
//EEEEEEEE
//EEEEXXXX
//EEEEEEEE

//EEEEEEEEEEEEEEE
//EEEEEEEEEEEEEEE
//EEEEOOOOOOOOOOE
//EEEEOO......OOE
//EEEEOO......OOE
//EEEEOOOOOOOOOOE
//EEEEEEEEEEEEEEE
//
//
//.............
//.....x.......
//.....x.......
//.xxxxxxxxxxx.
//.....x.....x.
//.....xxxxxxx.
//.............
//
//
//
//
//
