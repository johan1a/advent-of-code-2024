package se.johan1a.adventofcode2024

import se.johan1a.adventofcode2024.Utils.*

import scala.collection.mutable.ArrayBuffer

object Day15:

  def part1(input: Seq[String]): Long =
    val (grid, moves, start) = parse(input)
    var pos = start
    moves.foreach { move =>
      pos = applyMove(grid, pos, move)
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
        case 'O' =>
          100 * pos.y + pos.x
        case '[' =>
          100 * pos.y + pos.x
        case _ =>
          0
    ).sum

  def part2(input: Seq[String]): Long =
    val (smallGrid, moves, smallGridStart) = parse(input)
    val grid = enlargen(smallGrid)
    val start = Vec2(smallGridStart.x * 2, smallGridStart.y)
    var pos = start
    moves.foreach { move =>
      pos = applyMove2(grid, pos, move)
    }
    countGps(grid)

  private def applyMove2(grid: Grid, pos: Pos, move: Char) =
    val dir = Dir.fromChar(move)
    val nextPos = pos + dir
    var movedToPos = pos
    get(grid, nextPos) match
      case '.' =>
        set(grid, nextPos, '@')
        set(grid, pos, '.')
        movedToPos = nextPos
      case '#' =>
      case ch if ch == '[' || ch == ']' =>
        if canMoveTo(grid, nextPos, dir) then
          push(grid, nextPos, dir)
          set(grid, nextPos, '@')
          set(grid, pos, '.')
          movedToPos = nextPos
    movedToPos

  private def canMoveTo(grid: Grid, pos: Vec2, dir: Dir): Boolean =
    get(grid, pos) match
      case '.' => true
      case '#' => false
      case '[' if dir == Up || dir == Down =>
        canMoveTo(grid, pos + dir, dir) && canMoveTo(grid, Vec2(pos.x + 1, pos.y) + dir, dir)
      case ']' if dir == Up || dir == Down =>
        canMoveTo(grid, pos + dir, dir) && canMoveTo(grid, Vec2(pos.x - 1, pos.y) + dir, dir)
      case '[' =>
        canMoveTo(grid, Vec2(pos.x + 2, pos.y), dir)
      case ']' =>
        canMoveTo(grid, Vec2(pos.x - 2, pos.y), dir)

  private def push(grid: Grid, pos: Vec2, dir: Dir): Unit =
    get(grid, pos) match
      case '.' =>
      case '#' => throw new Exception("Can't push a wall")
      case '[' if dir == Up || dir == Down =>
        val nextPos = pos + dir
        push(grid, nextPos, dir)
        push(grid, Vec2(nextPos.x + 1, nextPos.y), dir)
        set(grid, nextPos, '[')
        set(grid, Vec2(nextPos.x + 1, nextPos.y), ']')
        set(grid, pos, '.')
        set(grid, Vec2(pos.x + 1, pos.y), '.')
      case ']' if dir == Up || dir == Down =>
        val nextPos = pos + dir
        push(grid, nextPos, dir)
        push(grid, Vec2(nextPos.x - 1, nextPos.y), dir)
        set(grid, nextPos, ']')
        set(grid, Vec2(nextPos.x - 1, nextPos.y), '[')
        set(grid, pos, '.')
        set(grid, Vec2(pos.x - 1, pos.y), '.')
      case '[' =>
        push(grid, Vec2(pos.x + 2, pos.y), dir)
        set(grid, pos, '.')
        set(grid, Vec2(pos.x + 1, pos.y), '[')
        set(grid, Vec2(pos.x + 2, pos.y), ']')
      case ']' =>
        push(grid, Vec2(pos.x - 2, pos.y), dir)
        set(grid, pos, '.')
        set(grid, Vec2(pos.x - 1, pos.y), ']')
        set(grid, Vec2(pos.x - 2, pos.y), '[')

  private def enlargen(grid: Grid) =
    val largeGrid = new ArrayBuffer[ArrayBuffer[Char]]()
    largeGrid.appendAll(grid.map { smallRow =>
      val row = new ArrayBuffer[Char]()
      row.appendAll(smallRow.flatMap { char =>
        char match
          case '#' => Seq('#', '#')
          case '.' => Seq('.', '.')
          case 'O' => Seq('[', ']')
          case '@' => Seq('@', '.')
      })
    })
    largeGrid

  private def parse(input: Seq[String]): (Grid, Array[Char], Pos) =
    val splitted = split(input)
    val grid = makeGrid(splitted.head)
    val moves = splitted.last.mkString("").toCharArray
    val start = find(grid, '@')
    (grid, moves, start.get)
