import scala.collection.mutable.ListBuffer

object BuildOrder {
  def main(args: Array[String]): Unit = {

    def builder(projects: List[Char], dependencies: List[(Char, Char)]): ListBuffer[Char] = {
      var newList = new ListBuffer[Char]() //mutable list to hold the build order to print.
      var w = 0 //increment variable for the while loop
      while(w <=projects.length - 2){
        //for items in the dependencies list:
        for (x <- 0 to dependencies.length - 1){
          //IF EVERYTHING IS ALREADY IN THE RIGHT ORDER:
          if(dependencies(x)._1 == (projects(w)) && dependencies(x)._2 == projects(w+1)){
            newList += projects(w)
          }
          //---------------------------------------------------------------------------------
          //FOR CASES WHERE EVERYTHING IS NOT ALREADY IN ORDER:
          else{
            val tup = dependencies(x)
            if (newList.contains(tup._1) || newList.contains(tup._2)){
              newList -= tup._2
              val indie = newList.indexOf(tup._1)
              newList = newList.patch(indie + 1, List(tup._2), 0)
            }
            else{
              newList += tup._1
              newList += tup._2
            }
          }//end of the second option's bracket
        }//END OF THE DEPENDENCIES CHECK INNER FOR-LOOP
        w += 1
        //FINAL CHECK TO ADD ELEMENTS IN THE LIST WHO DON'T HAVE ANY DEPENDENCIES:
        if(!newList.contains(projects(w))){
          newList += projects(w)
        }
        //if all of the items in the original list are already in the build order correctly, stop evaluating.
        if (newList == projects){
          w = 100
        }
      }//END OF OUTER FOR-LOOP
      newList //The mutable list with the build order is returned
    }

    //List of projects
    val projects = List('a', 'b', 'c', 'd', 'e', 'f')
    //Dependencies list of tuples
    val dependencies = List(('a', 'd'), ('f', 'b'), ('b', 'd'), ('f', 'a'), ('d', 'c'))
    //TEST FOR WHEN THE LIST ALREADY SATISFIES ALL DEPENDENCIES:
    //val dependencies = List(('a', 'b'), ('b', 'c'), ('c', 'd'), ('d', 'e'), ('e', 'f'))
    val answer = builder(projects, dependencies)

    val buildOrder = answer.mkString(", ")
    println("Output: " + buildOrder)
  }

}
