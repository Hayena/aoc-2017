import scala.io.Source

object InputParser {
  def fromResource[T](fileName: String)(implicit parser: StringParser[T]): Seq[T] = {
    val stream = getClass.getResourceAsStream(fileName)
    val source = Source.fromInputStream(stream)
    val lines = source.getLines().toSeq

    lines.map(parser.parse)
  }

  object AsInt {
    def unapply(s: String): Option[Int] =
      try {
        Some(s.toInt)
      } catch {
        case _: NumberFormatException => None
      }
  }
}

trait StringParser[T] { self =>
  def parse(s: String): T
}

object StringParser {
  def apply[T](f: String => T): StringParser[T] = (s: String) => f(s)
}
