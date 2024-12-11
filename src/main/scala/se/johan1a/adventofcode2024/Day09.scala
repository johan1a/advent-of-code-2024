package se.johan1a.adventofcode2024

object Day09:

  def part1(input: Seq[String]): Long =
    val line = input.head.toCharArray.map(_.toString.toInt)
    var sum = 0L
    var i = 0
    var j = line.length - 1
    var pos = 0
    while i <= j do
      if i % 2 == 0 then
        val id = i / 2
        0.until(line(i)).foreach { _ =>
          sum += pos * id
          pos += 1
        }
        i += 1
      else
        val id = j / 2
        while line(i) > 0 && line(j) > 0 do
          line(i) -= 1
          line(j) -= 1
          sum += pos * id
          pos += 1
        if line(j) == 0 then
          j -= 2
        if line(i) == 0 then
          i += 1
    sum

  case class File(id: Int, size: Int)

  case class Block(size: Int, files: Seq[File])

  def part2(input: Seq[String]): Long =
    val blocks = parse(input.head)
    val nbrBlocks = blocks.length
    var i = 0
    var j = blocks.length - 1
    while i <= j do
      var k = i
      assert(blocks(j).files.size == 1)
      val fileToMove = blocks(j).files.head
      while k < blocks.length && !fits(blocks(k), fileToMove) do
        k += 1
      if k < j then
        put(blocks, k, j)
      j -= 2
      if freeSpace(blocks(i)) == 0 then
        i += 1

    count(blocks)

  private def count(blocks: Array[Block]): Long =
    var pos = 0
    var sum = 0L
    blocks.indices.foreach: i =>
      val block = blocks(i)
      block.files.indices.foreach: j =>
        val file = block.files(j)
        0.until(file.size).foreach: _ =>
          sum += pos * file.id
          pos += 1
      pos += freeSpace(block)
    sum

  private def put(blocks: Array[Block], dest: Int, src: Int): Unit =
    val fileToMove: File = blocks(src).files.head
    val prevDestBlock = blocks(dest)
    val newDestBlock = Block(prevDestBlock.size, prevDestBlock.files :+ fileToMove)
    blocks(dest) = newDestBlock
    blocks(src) = Block(blocks(src).size, Seq.empty)

  private def fits(block: Block, file: File): Boolean = freeSpace(block) >= file.size

  private def freeSpace(block: Block) =
    val result = block.size - block.files.map(_.size).sum
    assert(result >= 0)
    result

  private def parse(input: String): Array[Block] =
    input.toCharArray.zipWithIndex.map { (char, i) =>
      val size = char.toString.toInt
      if i % 2 == 0 then
        val id = i / 2
        Block(size, Seq(File(id, size)))
      else
        Block(size, Seq.empty)
    }
