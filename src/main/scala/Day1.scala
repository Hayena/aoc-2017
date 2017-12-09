import scala.io.Source

object Day1 {
  import InputParser.AsInt

  def main(args: Array[String]): Unit = {
    val stream = getClass.getResourceAsStream("day1_input.txt")
    val source = Source.fromInputStream(stream)
    val line = source.getLines().toSeq.head
    val characters = line.toCharArray.toSeq
    val integers = characters.flatMap {
      case AsInt(i) => Some(i)
      case _ => None
    }

    println(s"Part One: ${sumEquals(integers, 1)}")
    println(s"Part Two: ${sumEquals(integers, integers.length / 2)}")
  }

  private def sumEquals(integers: Seq[Int], stepLength: Int): Int = {
    val integersWithIndex = integers.zipWithIndex

    integersWithIndex.foldLeft(0) {
      case (sum, (number, index)) =>
        val compareWithIndex = (index + stepLength) % integers.length
        val compareWithNumber = integers(compareWithIndex)

        if (number == compareWithNumber)
          sum + number
        else
          sum
    }
  }

}
