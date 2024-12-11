package se.johan1a.adventofcode2024

object Day09:

  def part1(input: Seq[String]): Long =
    val line = input.head.toCharArray().map(_.toString.toInt)
    var sum = 0L
    var i = 0
    var j = line.size - 1
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
    //printBlocks(blocks)
    val nbrBlocks = blocks.size
    var i = 0
    var j = blocks.size - 1
    while i <= j do
      var k = i
      assert(blocks(j).files.size == 1)
      val fileToMove = blocks(j).files.head
      while k < blocks.size && !fits(blocks(k), fileToMove) do
        k += 1
      if k < j then
        put(blocks, k, j)
      j -= 2
      if freeSpace(blocks(i)) == 0 then
        i += 1

    //printBlocks(blocks)
    count(blocks)

  private def count(blocks: Array[Block]): Long =
    var i = 0
    var pos = 0
    var sum = 0L
    while i < blocks.size do
      var j = 0
      val block = blocks(i)
      while j < block.files.size do
        var k = 0
        val file = block.files(j)
        while k < file.size do
          sum += pos * file.id
          pos += 1
          k += 1
        j += 1
      pos += freeSpace(block)
      i += 1
    sum

  private def put(blocks: Array[Block], dest: Int, src: Int) =
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

  private def printBlocks(blocks: Array[Day09.Block]) =
    var i = 0
    while i < blocks.size do
      var j = 0
      val block = blocks(i)
      while j < block.files.size do
        var k = 0
        val file = block.files(j)
        while k < file.size do
          print(file.id)
          k += 1
        j += 1
      0.until(freeSpace(block)).foreach { _ =>
        print(".")
      }
      i += 1
    println()

  private def parse(input: String): Array[Block] =
    input.toCharArray.zipWithIndex.map { (char, i) =>
      val size = char.toString.toInt
      if i % 2 == 0 then
        val id = i / 2
        Block(size, Seq(File(id, size)))
      else
        Block(size, Seq.empty)
    }.toArray
