package se.johan1a.adventofcode2024

import se.johan1a.adventofcode2024.Day17.parse as input
import se.johan1a.adventofcode2024.Utils.*

object Day17:

  type State = (Int, Long, Long, Long)

  def part1(input: Seq[String]): String =
    val (registers, instructions) = parse(input)
    run(registers, instructions).mkString(",")

  def part2(input: Seq[String]): Long =
    val (registers, instructions) = parse(input)
    var a = 0L
    var found = false
    var mostNbrMatching = 0L
    var mostNbrMatchingA = 0L
    while !found do
      val (allMatch, nbrMatching, output) = run2(a, instructions)

      found = allMatch

      if nbrMatching > mostNbrMatching && nbrMatching > 1 then
        println(s"i: $a, oct: ${a.toOctalString}, output ${output}, nbrMatching: $nbrMatching")
        mostNbrMatchingA = a
        mostNbrMatching = nbrMatching

      a = nextA(a, Math.max(0, mostNbrMatching - 1))

    mostNbrMatchingA

  private def run2(startA: Long, instructions: Seq[Long]) =
    val registers = Array(startA, 0, 0)
    val output = run(registers, instructions)
    var i = 0
    while i < output.size && i < instructions.size && output(i) == instructions(i) do
      i += 1
    (i == instructions.size && output.size == instructions.size, i, output.mkString(","))

  private def nextA(a: Long, nbrLockedDigits: Long) =
    val digits = toOctalDigitArray(a)
    val fixedDigits = digits.drop((digits.length - nbrLockedDigits).toInt)
    val leftDigits = digits.take(digits.length - fixedDigits.length)

    val increasedLeftPart = fromOctalDigitArray(leftDigits) + 1
    val increasedLeftPartDigits = toOctalDigitArray(increasedLeftPart)
    fromOctalDigitArray(increasedLeftPartDigits ++ fixedDigits)

  private def toOctalDigitArray(n: Long) =
    n.toOctalString.toCharArray.map(_.toString.toLong)

  private def fromOctalDigitArray(digits: Array[Long]) =
    java.lang.Long.parseLong(digits.mkString(""), 8)

  private val RegisterA = 0
  private val RegisterB = 1
  private val RegisterC = 2

  private def run(registers: Array[Long], instructions: Seq[Long]): Seq[Long] =
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

// while A != 0:
//   A = A / 8
//   print A % 8

//  Program: 0, 3, 5, 4, 3, 0
// (a0 / 8) % 8  == 0
// (a0 / 8^(k+1)) % 8  == xk
//
// 117440 / (Math.pow(8, 6).toInt) % 8
// scala> 117440 / (Math.pow(8, 1).toInt)
// val res4: Int = 14680

// scala> 117440 / (Math.pow(8, 2).toInt)
// val res5: Int = 1835

// scala> 117440 / (Math.pow(8, 2).toInt) % 8
// val res6: Int = 3

// scala> 117440 / (Math.pow(8, 3).toInt) % 8
// val res7: Int = 5

// scala> 117440 / (Math.pow(8, 4).toInt) % 8
// val res8: Int = 4

// scala> 117440 / (Math.pow(8, 5).toInt) % 8
// val res9: Int = 3

// scala> 117440 / (Math.pow(8, 6).toInt) % 8
// val res10: Int = 0

// input.txt, A = 8
// 0 B = 0 % 8 = 0
//
// 2 B = B xor 3
//
// 4 C = A / 2^B
//
// 6 B = B xor C
//
// 8 B = B xor 3
//
// 10A = A / 8
//
// 12print 0
//
// 14if A != 0
//    jump to a # we jumped, a was 1, b 1, c 0
//  else
//    sp += 2
//
// 0   B = A % 8
//
// 2   B = B xor 3
//
// 4   C = A / 2^B
//
// 6   B = B xor C
//
// 8   B = B xor 3
//
// 10  A = A / 2^3
//
// 12  print B % 8
//
// 14  if A != 0
//      jump to 0
//    else
//      end
// do
//  B = A % 8
//  B = B xor 3
//  C = A / 2^B
//  B = B xor C
//  B = B xor 3
//  A = A / 8
//  print(B % 8)
// while(A != 0)
//
//
// do
//  B = (A % 8) xor 3
//  B = (B xor (A / 2^B)) xor 3
//  A = A / 8
//  print(B % 8)
// while(A != 0)

// do
//  B = (A % 8) xor 3
//  B = ((((A % 8) xor 3) xor (A / 2^((A % 8) xor 3))) xor 3) % 8
//  A = A / 8
//  print(B % 8)
// while(A != 0)

//  0 = (((A % 8) xor 3) xor (A / 2^((A % 8) xor 3))) xor 3
