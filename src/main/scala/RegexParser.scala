import java.util

import scala.collection.mutable

class RegexParser {
  private val opStack = new mutable.Stack[Char]
  private val chStack = new mutable.Stack[Char]
  private val stateList = new util.ArrayList[State]
  private val transtitionList = new util.ArrayList[Transition]
  var stateIndex = 0
  val state = new State(stateIndex)


  def evaluateRegex(regex: String) = {

    if (regex.length() > 1) {
      for (i <- 0 until regex.length()) {
        val ch = regex.charAt(i)
        if (isChar(ch)) {
          if (chStack.isEmpty)
            chStack.push(ch)
          else {
            if (isChar(regex.charAt(i - 1))) {
              opStack.push('+')
              chStack.push(ch)
            } else
              chStack.push(ch)
          }
        } else {
          if (ch == '|') {
            opStack.push(ch)
          } else
            opStack.push(ch)
        }
        while (opStack.nonEmpty) {
          val initial = stateList.get(stateList.size() - 1)
          stateList.remove(stateList.size() - 1)
          opEval(initial)
        }
      }

    } else {
      val ch = regex.charAt(0)
      val t = new Transition(ch, 1)
      val state = new util.ArrayList[Integer]()
      val initial = stateList.get(stateList.size() - 1)
      stateList.remove(0)
      state.add(1)
      val s1 = new State(1)
      initial.transitions.add(t)
      stateList.add(initial)
      stateList.add(s1)
    }


  }

  private def opEval(initialState: State) {
    val op = opStack.pop()
    Character.valueOf(op) match {
      case '*' =>
        evalStar(initialState)
      case '+' =>
        evalConcat(initialState)

      case '|' =>
        evalUnion(initialState)
    }
  }

  private def evalUnion(initialState: State) {
    stateIndex = stateIndex + 1
    val state1 = new State(stateIndex)
    val secondExp = chStack.pop()
    val firstExp = chStack.pop()
    val trans1 = new Transition(firstExp, stateIndex)

    stateIndex = stateIndex + 1
    val state2 = new State(stateIndex)
    val trans2 = new Transition(secondExp, stateIndex)
    initialState.transitions.add(trans1)
    initialState.transitions.add(trans2)
    stateList.add(initialState)
    stateList.add(state1)
    stateList.add(state2)
  }

  private def evalConcat(initialState: State) {

    stateIndex = stateIndex + 1
    val state1 = new State(stateIndex)
    val secondExp = chStack.pop()
    val firstExp = chStack.pop()
    val trans1 = new Transition(firstExp, stateIndex)

    stateIndex = stateIndex + 1
    val state2 = new State(stateIndex)
    val trans2 = new Transition(secondExp, stateIndex)
    initialState.transitions.add(trans1)
    state1.transitions.add(trans2)
    stateList.add(initialState)
    stateList.add(state1)
    stateList.add(state2)
  }

  private def evalStar(initialState: State) {

    val exp = chStack.pop()
    stateIndex = stateIndex + 1
    val state1 = new State(stateIndex)
    val trans1 = new Transition(exp, stateIndex)
    val trans2 = new Transition(exp, stateIndex - 1)

    stateIndex = stateIndex + 1
    val state2 = new State(stateIndex)
    val trans3 = new Transition('_', stateIndex)
    val trans4 = new Transition('_', stateIndex)

    initialState.transitions.add(trans1)
    initialState.transitions.add(trans3)
    state1.transitions.add(trans2)
    state1.transitions.add(trans4)
    stateList.add(initialState)
    stateList.add(state1)
    stateList.add(state2)


  }


  private def isOperator(ch: Char): Boolean = {
    ch == '*' || ch == '|'
  }

  private def isLeftParentheses(ch: Char): Boolean = {
    ch == '('
  }

  private def isRightParentheses(ch: Char): Boolean = {
    ch == ')'
  }


  private def isChar(ch: Char): Boolean = {
    !isOperator(ch) && !isLeftParentheses(ch) && !isRightParentheses(ch)
  }
}
