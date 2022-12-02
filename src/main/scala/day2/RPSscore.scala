package day2
import java.io._

/**The object of RPSscore  is to calculate the sum of points recieved from a list of rock-paper-scissors matches.
 * For more information, see: https://adventofcode.com/2022/day/2
 */
object RPSscore extends App:
  /** convertData changes the strings given in the data, into numerical versions for further calculations.
   * The function assumes that no 'faulty' strings are provided.
   * @param v   The individual move performed by a player
   * @return    The numerical value the given move represents.
   */
  private def convertData(v: String): Int =
    if v == "A" || v == "X" then
      1
    else if v == "B" || v == "Y" then
      2
    else
      3
  end convertData

  /** determineScore converts the scores of player moves into the final score of the round.
   * @param opp   The score of the opponents move
   * @param own   The score of the players more
   * @return      The value of the round for the player
   */
  private def determineScore(opp: Int, own: Int) =
    if opp == 1 && own == 3 then
      val test = true
    val cmp = opp.compare(own)
    val winPoints = (if opp == own then
                      3
                    else if (own == 1 && opp == 3) || own -1 == opp then
                      6
                    else
                      0)
    own + winPoints
  end determineScore

  private def findScore() =
    //Initialising variables to read the file
    try
      val file = File("./src/main/scala/day2/data.txt")
      val fileReader = FileReader(file)
      val lineReader = BufferedReader(fileReader)
      //Attempt to read file data
      try
        var line = lineReader.readLine()
        var currScore = 0
        while line != null do
          val moves = line.split(" ", 2)
          val score = moves.map(x => convertData(x)).reduce((x, y) => determineScore(x, y))
          currScore += score
          println(s"${line}: ${score}")
          line = lineReader.readLine()
        currScore
      finally
        fileReader.close()
        lineReader.close()
    catch
      case notFound: FileNotFoundException => println("File wasn't connected correctly")
      case _ => println("Problems with reading the file")
  end findScore

  println(s"Total score of the match: ${findScore()}")

end RPSscore
