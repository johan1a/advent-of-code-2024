package se.johan1a.adventofcode2024

import se.johan1a.adventofcode2024.Utils.*

object Day17:

  type State = (Int, Long, Long, Long)

  def part1(input: Seq[String]): String =
    val (registers, instructions) = parse(input)
    run(registers, instructions).mkString(",")

  def part2(input: Seq[String]): Int =
    val (registers, instructions) = parse(input)
    var i = 0 //14110000
    val max = Int.MaxValue
    var output = Seq[Long]()
    while i < max && output != instructions do
      registers(0) = i
      registers(1) = 0
      registers(2) = 0
      output = run(registers, instructions)
      if i % 1000000 == 0 then
        println(s"i: $i, output: ${output}")
      i += 1
    i - 1

  private val RegisterA = 0
  private val RegisterB = 1
  private val RegisterC = 2

  private def run(registers: Array[Long], instructions: Seq[Long]) =
    var sp = 0
    var output = Seq[Long]()
    var seen: Set[State] = Set()
    while sp < instructions.size && !seen.contains((sp, registers.head, registers(1), registers(2))) && matches(
        output,
        instructions
      )
    do
      seen = seen + ((sp, registers.head, registers(1), registers(2)))
      val opcode = instructions(sp)
      val literalOperand = instructions(sp + 1)
      val comboOperand = getOperand(registers, literalOperand)
//      println(
//        s"sp $sp, registers: ${registers.toSeq}, opcode: $opcode, literalOperand: $literalOperand, comboOperand: $comboOperand"
//      )
      opcode match
        case 0 =>
          val a = registers(RegisterA)
          val b = Math.pow(2, comboOperand).toLong
          registers(RegisterA) = a / b
          sp += 2
        case 1 =>
          registers(RegisterB) = registers(RegisterB) ^ literalOperand
          sp += 2
        case 2 =>
          registers(RegisterB) = comboOperand % 8
          sp += 2
        case 3 =>
          if registers(RegisterA) != 0 then
            sp = literalOperand.toInt
          else
            sp += 2
        case 4 =>
          registers(RegisterB) = registers(RegisterB) ^ registers(RegisterC)
          sp += 2
        case 5 =>
          output = output :+ (comboOperand % 8)
          sp += 2
        case 6 =>
          val a = registers(RegisterA)
          val b = Math.pow(2, comboOperand).toLong
          registers(RegisterB) = a / b
          sp += 2
        case 7 =>
          val a = registers(RegisterA)
          val b = Math.pow(2, comboOperand).toLong
          registers(RegisterC) = a / b
          sp += 2

    output

  private def matches(output: Seq[Long], target: Seq[Long]) =
    output.zip(target).forall { case (a, b) => a == b }

  private def getOperand(registers: Array[Long], op: Long) =
    op match
      case n if n >= 0 && n <= 3 => n
      case 4                     => registers(RegisterA)
      case 5                     => registers(RegisterB)
      case 6                     => registers(RegisterC)
      case 7                     => ???

  private def parse(input: Seq[String]) =
    val splitted = split(input)
    val registers = splitted.head.map(l => numbers(l).head).toArray
    val instructions = numbers(splitted.last.last)
    (registers, instructions)
