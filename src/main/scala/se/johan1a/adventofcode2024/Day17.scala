package se.johan1a.adventofcode2024

import se.johan1a.adventofcode2024.Utils.*

object Day17:

  type State = (Int, Long, Long, Long)

  def part1(input: Seq[String]): String =
    val (registers, instructions) = parse(input)
    run(registers, instructions).mkString(",")

  def part2(input: Seq[String]): Long =
    val (registers, instructions) = parse(input)
    var i = Math.pow(8, instructions.size - 1).toLong // 1034000000
    val max = Long.MaxValue
    var output = Seq[Long]()
    while i < max && output != instructions do
      registers(0) = i
      registers(1) = 0
      registers(2) = 0
      output = run(registers, instructions)
      if i == 35184372088831L then
        var x = 3
      if i % 100000 == 0 then
        println(s"i: $i, output: ${output}")
      i += 1
    i - 1

  def part2b(input: Seq[String]): Long =
    val (registers, instructions) = parse(input)
    var i = Math.pow(8, instructions.size - 1).toLong // 1034000000
    println(s"start i: $i")
    var found = false
    while !found do
      found = run2(i, instructions)
//      if i % 1000000 == 0 then
//        println(s"i: $i")
      i += 1
    i - 1

  private def func(a: Long): Long =
    var b = (a % 8) ^ 3
    b = (b ^ (a / 2 ^ b)) ^ 3
    b % 8

  private def func2(a: Long): Long =
    var b = a % 8
    b = (b ^ (a / 2 ^ b)) ^ 3
    b % 8

  private def func3(a: Long): Long =
    if a % 2 == 0 then
      (a + 3) % 8
    else
      (a + 4) % 8

  def func4(a: Long) =
    (8 - ((a + 10) / 2) % 8) % 8

  var best = -1

  private def run2(startA: Long, instructions: Seq[Long]) =
    var continue = true
    var i = 0
    var a = startA
    while i < instructions.size && continue && a != 0 do
      val b = func4(a)
      if b != instructions(i) then
        continue = false
      a = a / 8
      i += 1
    if i > best then
      best = i
      println(s"best: $best at A: $startA")
    continue && i == instructions.size
//  Register A: 2024
//  Register B: 0
//  Register C: 0
//
//  Program: 0, 3, 5, 4, 3, 0
//  Program: div, 3, out, 4, jump, 0

// to print 0:
  // output = output :+ (comboOperand % 8)
  // ins must be 5,
  // comboOperand % 8 must be 0
  // -> literal op must be 0
  // or 4 && A == 0,
  // or 5 && B == 0,
  // or 6 && C ==0

  // ((X / 8) % 8 ) == 0

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

// 000 0
// 001 1
// 010 2
// 011 3
// 100 4
// 101 5
// 110 6
// 111 7
