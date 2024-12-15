package se.johan1a.adventofcode2024

import se.johan1a.adventofcode2024.Utils.{Vec2, numbers}

object Day14:

  case class Robot(p: Vec2, v: Vec2)

  def part1(input: Seq[String], t: Int = 100, w: Int = 101, h: Int = 103): Int =
    var robots = parse(input)
    0.until(t).foreach { _ =>
      robots = simulate(robots, w, h)
    }
    safetyFactor(robots, w, h)

  private def simulate(robots: Seq[Robot], w: Int, h: Int) =
    robots.map { robot =>
      val pos = Vec2(
        wrap(robot.p.x, robot.v.x, w),
        wrap(robot.p.y, robot.v.y, h)
      )
      Robot(pos, robot.v)
    }

  private def wrap(p: Long, v: Long, w: Int) =
    (p + v + w) % w

  private def safetyFactor(robots: Seq[Robot], w: Int, h: Int) =
    val quadrants = Array(0, 0, 0, 0)
    val midX = w / 2
    val midY = h / 2
    robots.foreach { robot =>
      if robot.p.x < midX && robot.p.y < midY then
        quadrants(0) += 1
      else if robot.p.x > midX && robot.p.y < midY then
        quadrants(1) += 1
      else if robot.p.x < midX && robot.p.y > midY then
        quadrants(2) += 1
      else if robot.p.x > midX && robot.p.y > midY then
        quadrants(3) += 1
    }
    quadrants.product

  def part2(input: Seq[String], t: Int = 10000000, w: Int = 101, h: Int = 103): Int =
    var robots = parse(input)
    var i = 0
    var hasResult = false
    while i < t && !hasResult do
      if i % 1000 == 0 then
        println(s"i: $i")
      robots = simulate(robots, w, h)
      hasResult = findTree(robots, w, h)
      i += 1
    i

  private def findTree(robots: Seq[Day14.Robot], w: Int, h: Int) =
    val positions = robots.map(_.p).toSet
    var result = false
    var rows = Seq[String]()
    0.until(h).foreach { y =>
      val string = 0.until(w).map { x =>
        if positions.contains(Vec2(x, y)) then
          "#"
        else
          "."
      }.mkString("")
      rows = rows :+ string
      if string.contains("########") then
        result = true
    }
    if result then
      rows.foreach(println)
    result

  private def parse(input: Seq[String]): Seq[Robot] =
    input.map { line =>
      val nn = numbers(line)
      Robot(Vec2(nn(0), nn(1)), Vec2(nn(2), nn(3)))
    }