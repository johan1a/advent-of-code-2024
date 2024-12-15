package se.johan1a.adventofcode2024

import se.johan1a.adventofcode2024.Utils.*

object Day15:

  def part1(input: Seq[String]): Long =
    val (grid, moves, start) = parse(input)
    var pos = start
    moves.foreach { move =>
      pos = applyMove(grid, pos, move)
      //printGrid(grid)
    }
    countGps(grid)

  private def applyMove(grid: Grid, pos: Pos, move: Char) =
    val dir = Dir.fromChar(move)
    val nextPos = pos + dir
    var movedToPos = pos
    get(grid, nextPos) match
      case '.' =>
        set(grid, nextPos, '@')
        set(grid, pos, '.')
        movedToPos = nextPos
      case '#' =>
      case 'O' =>
        var after = nextPos
        while get(grid, after) == 'O' do
          after = after + dir
        if get(grid, after) == '.' then
          set(grid, after, 'O')
          set(grid, pos, '.')
          set(grid, nextPos, '@')
          movedToPos = nextPos
    movedToPos

  private def countGps(grid: Grid) =
    allPositions(grid).map(pos =>
      get(grid, pos) match
        case 'O' => 100 * pos.y + pos.x
        case _   => 0
    ).sum

  def part2(input: Seq[String]): Int =
    -1

  private def parse(input: Seq[String]): (Grid, Array[Char], Pos) =
    val splitted = split(input)
    val grid = makeGrid(splitted.head)
    val moves = splitted.last.mkString("").toCharArray
    val start = find(grid, '@')
    (grid, moves, start.get)
