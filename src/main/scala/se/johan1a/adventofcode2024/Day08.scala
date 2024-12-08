package se.johan1a.adventofcode2024

import se.johan1a.adventofcode2024.Utils.*

object Day08:

  case class Antenna(pos: Vec2, char: Char)

  def part1(input: Seq[String]): Int =
    val (grid, antennas) = parse(input)
    getAntiNodes(grid, antennas).size

  def part2(input: Seq[String]): Int =
    -1

  private def getAntiNodes(grid: Grid, antennas: Seq[Antenna]): Set[Vec2] =
    val types = antennas.map(_.char).distinct
    var antinodes = Set[Vec2]()
    types.foreach { antennaType =>
      val pairs = getPairs(antennas, antennaType)
      pairs.foreach { pair =>
        val direction = getDirection(pair._1, pair._2)
        val antinode0 = pair._2.pos + direction
        val antinode1 = pair._1.pos - direction
        antinodes = antinodes + antinode0
        antinodes = antinodes + antinode1
      }
    }
    antinodes.filter(inRange(grid, _))

  private def getDirection(a: Antenna, b: Antenna): Vec2 =
    b.pos - a.pos

  private def getPairs(antennas: Seq[Antenna], antennaType: Char): Seq[(Antenna, Antenna)] =
    var pairs = Seq[(Antenna, Antenna)]()
    val sameType = antennas.filter(_.char == antennaType)
    sameType.indices.foreach { i =>
      (i + 1).until(sameType.size).foreach { j =>
        pairs = pairs :+ (sameType(i), sameType(j))
      }
    }
    pairs

  private def parse(input: Seq[String]) =
    val grid = makeGrid(input)
    val antennas = grid.indices.flatMap { row =>
      grid.head.indices.map { col =>
        if grid(row)(col) != '.' then
          Some(Antenna(Vec2(col, row), grid(row)(col)))
        else None
      }.collect { case Some(a) => a }
    }
    (grid, antennas)
