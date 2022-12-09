package day7
import java.io._

/** The purpose of dirNavigator is to recursively go through a set of git-commands,
 * and determine the sizes of directories in the system.
 * For more information, see: https://adventofcode.com/2022/day/7*/
object dirNavigator  extends App:

  /** sizeSum goes through the data, calculates the sizes of the directories, and reports the desired sum.
   * @param first   A variable determining which half of the exercise is being evaluated
   * @return        The sum of directories with size less than 100 000.
   */
  private def sizeSum(first: Boolean) =
    try
      val file = File("./src/main/scala/day7/data.txt")
      val fileReader = FileReader(file)
      val lineReader = BufferedReader(fileReader)
      //Attempt to read file data
      try
        //Keep track of the sizes of all the directories
        var dirSizes = scala.collection.mutable.ArrayBuffer[Long]()
        /** traverseFiles goes through the given commands, and stores the sizes of the directories in the above ArrayBuffer
         * @return  Whether the entire file has been gone through, the size of the child directory
         */
        def traverseFiles(): (Boolean, Long) =
          var lookingAt = lineReader.readLine()
          //Alerts if the instructions say to move back a directory
          var linesDone = false
          var done = false
          var childSpace = 0.toLong
          while lookingAt != null && !done && !linesDone do
            val splitInfo = lookingAt.split(" ")
            //Moving to a different directory
            if splitInfo(1) == "cd" then
              //Moving back or forwards?
              if splitInfo(2) == ".." then
                done = true
              else
                val dirVisit = traverseFiles()
                linesDone = dirVisit._1
                childSpace += dirVisit._2
            //Adding a file, add its size to the directory size
            else if splitInfo(0).toIntOption.isDefined then
              childSpace += splitInfo(0).toLong
            //Otherwise we're adding a directory or listing files, which are not relevant for the execution of the code
            if !done && !linesDone then
              lookingAt = lineReader.readLine()
          //Add the size of the directory to the list of all sizes
          dirSizes.append(childSpace)
          //Return, and inform the previous iteration of whether lineReader can be called again
          if linesDone || lookingAt == null then
            (true, childSpace)
          else
            (false, childSpace)
        end traverseFiles

        //Go through the commands
        traverseFiles()
        //In the first half of the exercise, the sum of the sizes of directories with size at most 100000 is desired.
        if first then
          dirSizes.filter(_ <= 100000).sum
        /** In the second half, the size of the smallest individual directory, that can be removed to have
         * 30 000 000 empty space out of a total of 70 000 000 */
        else
          val spaceToFree = dirSizes.max - (70000000 - 30000000)
          dirSizes.filter(_ >= spaceToFree).min

      finally
        fileReader.close()
        lineReader.close()
    catch
      case notFound: FileNotFoundException => println("File wasn't connected correctly")
      case _ => println("Problems with reading the file")
  end sizeSum

  println(s"The sum of the directories with size 100 000 or less: ${sizeSum(true)}")
  println(s"The size of the smallest directory that can be deleted to achieve 30 000 000 empty space: ${sizeSum(false)}")

end dirNavigator

