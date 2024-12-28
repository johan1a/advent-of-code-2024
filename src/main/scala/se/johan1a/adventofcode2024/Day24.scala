package se.johan1a.adventofcode2024

import se.johan1a.adventofcode2024.Utils.{split, splitOnce}

object Day24:

  sealed trait Op:
    def ref: String

  case class Literal(ref: String, value: Boolean) extends Op
  case class And(ref: String, a: String, b: String) extends Op
  case class Or(ref: String, a: String, b: String) extends Op
  case class Xor(ref: String, a: String, b: String) extends Op

  def part1(input: Seq[String]): Long =
    val ops = parse(input).map(op => op.ref -> op).toMap
    val zRefs = ops.filter(_._1.startsWith("z")).keys.toSeq.sorted.reverse
    val bits = zRefs.map(ref => compute(ops, ref)).map(b => if b then "1" else "0").mkString
    java.lang.Long.parseLong(bits, 2)

  def part2(input: Seq[String]): Int =
    -1

  private def compute(refs: Map[String, Op], ref: String): Boolean =
    refs(ref) match
      case Literal(ref, value) => value
      case And(ref, a, b)      => compute(refs, a) && compute(refs, b)
      case Or(ref, a, b)       => compute(refs, a) || compute(refs, b)
      case Xor(ref, a, b)      => compute(refs, a) ^ compute(refs, b)

  private def parse(input: Seq[String]): Seq[Op] =
    val splitted = split(input)
    val literals = splitted.head.map(parseLiteral)
    val gates = splitted.last.map(parseGate)
    gates ++ literals

  private def parseLiteral(str: String): Literal =
    val (ref, value) = splitOnce(str, ": ")
    Literal(ref, value == "1")

  private def parseGate(str: String): Op = str match
    case s"$a AND $b -> $ref" => And(ref, a, b)
    case s"$a OR $b -> $ref"  => Or(ref, a, b)
    case s"$a XOR $b -> $ref" => Xor(ref, a, b)
