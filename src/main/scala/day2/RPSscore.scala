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

  /** determineWinStatus is used in the second half of the excercise, and determines what the
   * player needs to play in order to achieve the desired result. Following this, the method
   * returns the score of the round.
   * @param opp     The value of the opponents move
   * @param letter  The desired win status
   * @return        The score gathered from the round*/
  private def determineWinStatus(opp: Int, letter: String) =
    val ownVal =  (if letter == "X" then
                    if opp == 1 then 3 else opp - 1
                  else if letter == "Y" then
                    opp
                  else
                    if opp == 3 then 1 else opp +1)
    determineScore(opp, ownVal)
  end determineWinStatus


  /** findScore can execute either the first or second half of the exercise. Most of the functionality is in the abuve functions.
   * @param round1  A variable determining whether the first or second half of the exercise is being executed
   * @return        The score of the match*/
  private def findScore(round1: Boolean) =
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
          val score = (if round1 then
                        moves.map(x => convertData(x)).reduce((x, y) => determineScore(x, y))
                      else
                        determineWinStatus(convertData(moves(0)), moves(1))
                      )
          currScore += score
          line = lineReader.readLine()
        currScore
      finally
        fileReader.close()
        lineReader.close()
    catch
      case notFound: FileNotFoundException => println("File wasn't connected correctly")
      case _ => println("Problems with reading the file")
  end findScore

  println(s"Total score of the match given moves: ${findScore(true)}")
  println(s"Total score of the match given win status: ${findScore(false)}")

end RPSscore
