import enumeratum.{ Enum, EnumEntry }

object Day8 {
  type Address = String
  type Value = Int
  type Register = Map[Address, Value]

  private val initialState = State.initial

  def main(args: Array[String]): Unit = {
    val latestState = InputParser
      .fromResource[Entry]("day8_input.txt")
      .foldLeft(initialState) {
        case (state, entry) =>
          val newState = processEntry(state, entry)

          newState
      }

    println(s"The largest value in the register is ${latestState.values.max}")
    println(s"The largest value to every be in the register is ${latestState.largestValue}")
  }

  private def processEntry(state: State, entry: Entry): State =
    if(entry.equality.compare(state.getValue(entry.compareWithAddress), entry.compareWithValue))
      updateState(state, entry)
    else
      state

  private def updateState(state: State, entry: Entry): State = {
    val currentValue = state.getValue(entry.adjustByAddress)
    val newValue = adjustValue(currentValue, entry.adjustByValue, entry.action)

    state + (entry.adjustByAddress -> newValue)
  }

  private def adjustValue(value: Value, adjustBy: Value, action: Action): Value = {
    action match {
      case Action.Increment =>
        value + adjustBy
      case Action.Decrement =>
        value - adjustBy
    }

  }

  case class State(
    register: Register,
    largestValue: Value
  ) {

    def getValue(address: Address): Value = register(address)
    def +(entry: (Address, Value)): State = {
      val x = register + entry

      State(
        register = x,
        largestValue = Seq(largestValue, entry._2).max
      )
    }

    def values: Seq[Value] = register.values.toSeq
  }

  object State {
    private val defaultValue = 0

    def initial: State = State(
      register = Map.empty.withDefaultValue(defaultValue),
      largestValue = defaultValue
    )
  }
}

sealed abstract class Action(override val entryName: String) extends EnumEntry

object Action extends Enum[Action] {
  val values = findValues

  case object Increment extends Action("inc")
  case object Decrement extends Action("dec")

  def unapply(s: String): Option[Action] = withNameInsensitiveOption(s)
}

sealed abstract class Equality(override val entryName: String) extends EnumEntry {

  def compare(value: Int, compareWith: Int): Boolean
}

object Equality extends Enum[Equality] {
  val values = findValues

  case object LargerThan extends Equality(">") {
    def compare(value: Int, compareWith: Int): Boolean = value > compareWith
  }

  case object LargerThanOrEqual extends Equality(">=") {
    def compare(value: Int, compareWith: Int): Boolean = value >= compareWith
  }

  case object SmallerThan extends Equality("<") {
    def compare(value: Int, compareWith: Int): Boolean = value < compareWith
  }

  case object SmallerThanOrEqual extends Equality("<=") {
    def compare(value: Int, compareWith: Int): Boolean = value <= compareWith
  }

  case object Equal extends Equality("==") {
    def compare(value: Int, compareWith: Int): Boolean = value == compareWith
  }

  case object NotEqual extends Equality("!=") {
    def compare(value: Int, compareWith: Int): Boolean = value != compareWith
  }

  def unapply(s: String): Option[Equality] = withNameInsensitiveOption(s)
}

case class Entry(
  adjustByAddress: String,
  action: Action,
  adjustByValue: Int,
  compareWithAddress: String,
  equality: Equality,
  compareWithValue: Int
)
object Entry {
  import InputParser._

  val regex = "([a-z].*) (inc|dec) (-?[0-9].*) if ([a-z].*) (>|>=|<|<=|==|!=) (-?[0-9].*)".r

  implicit val parser: StringParser[Entry] = StringParser {
    case regex(adjustByAddress, Action(action), AsInt(adjustByValue), compareWithAddress, Equality(equality), AsInt(compareWith)) =>
      Entry(
        adjustByAddress = adjustByAddress,
        action = action,
        adjustByValue = adjustByValue,
        compareWithAddress = compareWithAddress,
        equality = equality,
        compareWithValue = compareWith
      )
  }
}
