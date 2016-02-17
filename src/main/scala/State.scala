import java.util


/* State stores state index and all possible transitions
   from that state depending on following character */
class State(stateName: Int) {
  private val state = this.stateName
  var transitions = new util.ArrayList[Transition]

  override def toString: String = {
    val transitionsString = transitions.toString
    s"State:$state; Transitions:$transitionsString"
  }
}

//case class to store a single transition depending on transitionChar to a particular state
case class Transition(transitionChar: Char, stateName: Int)