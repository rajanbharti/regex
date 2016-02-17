import java.util

class State(stateName: Int) {
  private var state = this.stateName
  var transitions = new util.ArrayList[Transition]
}

case class Transition(transitionChar: Char, stateName: Int)