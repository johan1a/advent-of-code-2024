package se.johan1a.adventofcode2024

import se.johan1a.adventofcode2024.Utils.*

object Day08:

  case class Antenna(pos: Vec2, char: Char)

  def part1(input: Seq[String]): Int =
    val (grid, antennas) = parse(input)
    getAntiNodes(grid, antennas).size

  def part2(input: Seq[String]): Int =
    val (grid, antennas) = parse(input)
    getAntiNodes2(grid, antennas).size

  private def getAntiNodes(grid: Grid, antennas: Seq[Antenna]): Set[Vec2] =
    val types = antennas.map(_.char).distinct
    var antinodes = Set[Vec2]()
    types.foreach { antennaType =>
      getPairs(antennas, antennaType).foreach { pair =>
        val direction = pair._2.pos - pair._1.pos
        val antinode0 = pair._2.pos + direction
        val antinode1 = pair._1.pos - direction
        antinodes = antinodes + antinode0
        antinodes = antinodes + antinode1
      }
    }
    antinodes.filter(inRange(grid, _))

  private def getAntiNodes2(grid: Grid, antennas: Seq[Antenna]): Set[Vec2] =
    val types = antennas.map(_.char).distinct
    var antinodes = Set[Vec2]()
    types.foreach { antennaType =>
      getPairs(antennas, antennaType).foreach { pair =>
        val direction = pair._2.pos - pair._1.pos
        var antinode = pair._2.pos
        while (inRange(grid, antinode)) {
          antinodes = antinodes + antinode
          antinode = antinode + direction
        }

        antinode = pair._1.pos
        while (inRange(grid, antinode)) {
          antinodes = antinodes + antinode
          antinode = antinode - direction
        }
      }
    }
    antinodes.filter(inRange(grid, _))

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
