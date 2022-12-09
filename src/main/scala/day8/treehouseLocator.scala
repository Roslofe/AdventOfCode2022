package day8
import java.io._
import scala.collection.mutable.ListBuffer
import scala.math.max
import scala.math.min

/** The purpose of treehouseLocator is to examine the heights of trees, to determine which are visible from outside the forest.
 * For more information, see: https://adventofcode.com/2022/day/8 */
object treehouseLocator extends App:

  /** The function initially goes through all trees, and stores their heights in a ListBuffer, after which it goes through all the trees to determine
   * if they are visible from outside. Finally, it returns the number of visible trees.
   * In the second half, it finds the maximum scenic score, which is a multiplication between the number of trees that can be seen from each tree.
   * @param first   Determines which half of the exercise is being executed
   * @return        The number of trees visible from outside the forest
   */
  private def locateTree(first: Boolean) =
    try
      val file = File("./src/main/scala/day8/data.txt")
      val fileReader = FileReader(file)
      val lineReader = BufferedReader(fileReader)
      //Attempt to read file data
      try
        //Add all sizes of trees here, works as a "coordinate system"
        val treeHeights = ListBuffer[Array[Int]]()
        //Initialise the sizes of the trees by going through the data
        var line = lineReader.readLine()
        while line != null do
          treeHeights.append(line.toCharArray.map(_.toString.toInt))
          line = lineReader.readLine()
        val rows = treeHeights.length
        val columns = treeHeights(0).length

        /** When given the coordinates of a tree, the function produces the trees that might be blocking the tree from all directions.
         * The tallest tree from each direction is found, and the smaller tree from each axis (up-down, left-right) is returned.
         * @param y   The row of the tree being examined
         * @param x   The column of the tree being examined
         * @return    The trees which could cause the tree to not be visible
         */
        def highest(y: Int, x: Int): (Int, Int) =
          //Heights of the tallest trees at the front and the back
          val front = treeHeights.take(y).map(_(x)).max
          val back = treeHeights.takeRight(rows - (y + 1)).map(_(x)).max
          //Same operation, but for the horizontal trees
          val left = treeHeights(y).take(x).max
          val right = treeHeights(y).takeRight(columns - (x + 1)).max
          //Return the smaller vertical and horizontal values
          (min(front, back), min(left, right))
        end highest

        def sceneScore(y: Int, x: Int): Int =
          if y == 0 || x == 0 then
            0
          else
            val current = treeHeights(y)(x)
            /** Determines the multiplier based on the comparisons done between the heights
             * @param cmps  An array containing data on how the heights compare to the current tree
             * @return      The multiplier used for calculating the scenic score
             */
            def determineMult(cmps: Array[Int]) = (if cmps.contains(0) || cmps.contains(-1) then cmps.indexWhere(z => z == -1 || z == 0) + 1 else cmps.length)
            //Number of trees that can be seen in each direction before the view is blocked
            val front = treeHeights.take(y).map(z => current.compare(z(x))).reverse
            val frontMult = determineMult(front.toArray)
            val back = treeHeights.takeRight(rows - (y + 1)).map(a => current.compare(a(x)))
            val backMult = determineMult(back.toArray)
            val left = treeHeights(y).take(x).map(a => current.compare(a)).reverse
            val leftMult = determineMult(left)
            val right = treeHeights(y).takeRight(columns - (x + 1)).map(a => current.compare(a))
            val rightMult = determineMult(right)
            frontMult * backMult * leftMult * rightMult
        end sceneScore

        if first then
          //Initialise the values of whether a tree is visible
          val treesVisible = Array.fill(rows)(Array.fill(columns)(false))
          //All trees at the edges are visible
          treesVisible.update(0, Array.fill(columns)(true))
          treesVisible.update(rows - 1, Array.fill(columns)(true))
          var i = 1
          while i < rows - 1 do
            treesVisible(i).update(0, true)
            treesVisible(i).update(columns - 1, true)
            i += 1
          //Go through the remaining trees, check if they are visible
          var v = 1
          while v < rows - 1 do
            var x = 1
            while x < columns - 1 do
              val current = treeHeights(v)(x)
              //Is it visible from somewhere?
              val highestNeigh = highest(v, x)
              if highestNeigh._1 < current || highestNeigh._2 < current then
                treesVisible(v).update(x, true)
              x += 1
            v += 1
          //Return the number of visible trees
          treesVisible.flatten.count(_ == true)
        else
          //Keep track of the scenic scores of the trees
          val scenicScores = Array.fill(rows)(Array.fill(columns)(0))
          //Go through each tree, set scenic score
          var v = 0
          while v < rows do
            var x = 0
            while x < columns do
              val score = sceneScore(v, x)
              scenicScores(v).update(x, score)
              x += 1
            v += 1
          //Get the largest scenic score
          scenicScores.flatten.sorted(Ordering.Int.reverse)(0)
      finally
        fileReader.close()
        lineReader.close()
    catch
      case notFound: FileNotFoundException => println("File wasn't connected correctly")
      case _ => println("Problems with reading the file")
  end locateTree

  println(s"Number of trees visible: ${locateTree(true)}")
  println(s"Highest scenic score: ${locateTree(false)}")

end treehouseLocator

