package day11
import java.io._
import scala.collection.mutable.ListBuffer

/** A class representing each monkey.
 * @param initItems   The items carried by the monkey at the start of the game
 * @param worry       The transformation applied to the worry score of an item
 * @param throwTest   The test applied to the worry score, to determine which monkey it gets thrown to
 * @param throwTo     The index of the monkey that the item will be thrown to, based on the test's outcome
 */
case class Monkey(val initItems: ListBuffer[Int], worry: Int => Int, throwTest: Int => Boolean, throwTo: (Int, Int)):
  val items = initItems
  var inspectionNum = 0
  private def throwItem(item: Int) =
    if throwTest(item) then
      (item, throwTo._1)
    else
      (item, throwTo._2)
  end throwItem

  def inspectItem(item: Int) =
    val newWorry = worry(item) / 3
    items -= item
    inspectionNum += 1
    throwItem(newWorry)

end Monkey

object shenaniganCounter extends App:

  /** trowItems passes items between monkeys, and returns the so-called level of monkey business. 
   * For more information, see: https://adventofcode.com/2022/day/11 */
  private def throwItems() =
    try
      val file = File("./src/main/scala/day11/data.txt")
      val fileReader = FileReader(file)
      val lineReader = BufferedReader(fileReader)
      //Attempt to read file data
      try
        var line = lineReader.readLine()
        //Store the lines
        val allLines = ListBuffer[String]()
        while line != null do
          if line != "" then
            allLines += line.trim
          line = lineReader.readLine()
        //Get the lines of each monkey. Each has 6 lines of information
        val monkeyLines = allLines.grouped(6).toArray
        //Store all the monkeys, in order of introduction. The index of the monkey serves as its name
        var allMonkeys = Array[Monkey]()
        for lineSet <- monkeyLines do
          //Get the items the monkey is initially holding
          val items = lineSet(1).split(": ")(1).split(", ").map(_.toInt)
          val operationInfo = lineSet(2).split(" = ")(1).split(" ")
          //A horrendous line of code to get the operation performed to the worry level
          val operation: Int => Int = old => if operationInfo(1) == "+" then old + (if operationInfo(2) != "old" then operationInfo(2).toInt else old) else old * (if operationInfo(2) != "old" then operationInfo(2).toInt else old)
          //Testing if the worry level is divisible
          val dividingTest: Int => Boolean = x => x % lineSet(3).split(" by ")(1).toInt == 0
          //What monkey will the item be thrown to?
          val throwTo = (lineSet(4).split(" monkey ")(1).toInt, lineSet(5).split(" monkey ")(1).toInt)
          allMonkeys = allMonkeys :+ Monkey(ListBuffer(items: _ *), operation, dividingTest, throwTo)
        var round = 1
        while round <= 20 do
          //Each monkey investigates all it's items, and they are moved to the relevant monkey
          var i = 0
          while i < allMonkeys.length do
            val current = allMonkeys(i)
            while current.items.nonEmpty do
              val toMove = current.inspectItem(current.items.head)
              allMonkeys(toMove._2).items += toMove._1
            i += 1
          round += 1
        //Return the level of "monkey business", a.k.a the product of the number of items inspected by the two monkeys who inspected the most items
        allMonkeys.map(_.inspectionNum).sorted.takeRight(2).product

      finally
        fileReader.close()
        lineReader.close()
    catch
      case notFound: FileNotFoundException => println("File wasn't connected correctly")
      case _ => println("Problems with reading the file")
  end throwItems

  println(s"Level of monkey business after 20 rounds: ${createMonkeys()}")

end shenaniganCounter

