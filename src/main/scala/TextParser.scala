import java.util

class TextParser extends RegexParser {

  //parses text for given regular expression
  def parseText(text: String, regex: String): String = {
    evaluateRegex(regex)
    var location: Int = 0
    var allLocations: String = ""
    val stateList = getStateList
    var stateIndex: Int = 0
    var count: Int = 0
    for (i <- 0 until text.length) {
      val ch = text.charAt(i)
      for (j <- 0 until stateList.get(stateIndex).transitions.size()) {
        if (ch == stateList.get(stateIndex).transitions.get(j).transitionChar) {
          val tempTrans = stateList.get(stateIndex).transitions.get(j)
          stateIndex = tempTrans.stateName
          count = count + 1
          if (stateList.get(stateIndex).transitions.isEmpty) {
            if (count > 1)
              location = i - count + 1
            else
              location = i
            allLocations = allLocations + Integer.toString(location) + " "
            count = 0
            stateIndex = 0
          }
        }
      }
    }
    allLocations
  }
}
