
object Day2 {
  def main(args: Array[String]): Unit = {
    val rows = InputParser.fromResource[Row]("day2_input.txt")

    println("Part One: " + rows.map(row => row.columns.max - row.columns.min).sum)
    println("Part Two: " + rows.map(row => getLargestEvenlyDividableDivision(row.columns)).sum)
  }

  private def getLargestEvenlyDividableDivision(columns: Seq[Int]): Int = {
    def foo(x: Int, y: Seq[Int]): Int =
      y.find(z => x % z == 0) match {
        case Some(z)           =>
          println(s"Found match in $x & $z")
          x / z
        case None if y.isEmpty => 0
        case None              => foo(y.head, y.tail)
      }

    val sortedColumns = columns.sorted.reverse

    val result = foo(sortedColumns.head, sortedColumns.tail)

    println(result)

    result
  }
}

case class Row(columns: Seq[Int])

object Row {
  import InputParser.AsInt

  implicit val parser: StringParser[Row] = StringParser { line =>
    val columns = line.split("\t").map {
      case AsInt(i) => i
    }

    Row(columns.toSeq)
  }
}
