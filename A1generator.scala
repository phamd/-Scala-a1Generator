import java.io.File
import java.io.FileWriter
import java.io.BufferedReader
import java.io.PrintWriter
import java.util.ArrayList
import java.util.Arrays
import java.util.Collections
import scala.io.Source
import Array._
import java.io.FileNotFoundException
import java.io.IOException


object A1Generator {

  
	val minimumBreak = -16		// Min breakpoint for A1
	val maximumBreak = 16		// Max breakpoint for A1
	val minimumResult = 0		// Min return value for A1
	val maximumResult = 12		// Max return value for A1
	val minResultRange = -24	// Min param number that can be found in A1Test
	val maxResultRange = 24		// Max param number that can be found in A1Test
	val numberOfCases = 20		// Number of Test cases for A1Test
	
	/**
	 * @param args
	 */
	
	def main(args: Array[String]) {
		generateAssignment1
	}
	
	
	/**
	 * Generates the whole Assignment1
	 */
	def generateAssignment1 {
	  
		var file:String = "students.txt" // students.txt is placed in project root	
		var studentNumber:String = null
		
		try {
		  
			for( studentNumber <- Source.fromFile(file).getLines ) {
				outputA1Tests(studentNumber, genBreakpoints, genResults)
				outputA2Tests(studentNumber, genNumbersForA2)
			}
			
			println("done")
			
		} catch {
		  
			case e: FileNotFoundException => {
				createStudentsTxt(file) // create students file
				generateAssignment1 // Call the generator again with the newly created file
			}
			case e: Exception => println("exception caught: " + e)
		}
	}

	
	/**
	 * Fills student.txt with random student numbers for testing
	 * @param file students.txt
	 */
	private def createStudentsTxt(file:String) {
	  
	  	val newFile = new File(file).createNewFile
		val fileWriter = new FileWriter(file)
		val printWriter = new PrintWriter(fileWriter)
		
		for ( i <- 0 to 8) {
		  
			printWriter.println(randInt(1000000,1300000))

		}
		printWriter.close()
		println(file +" has been created for you")
	}


	/**
	 * @return the 7 breakpoint values for each of the branches of the tree
	 */
	private def genBreakpoints: Array[Int] =  {

		var minimumBreak:Int = this.minimumBreak
		var maximumBreak:Int = this.maximumBreak
		var cases:Array[Int] = Array(0,0,0,0,0,0,0)

		for ( i <- 0 to cases.length - 1) {
			
			cases(i) = randInt(minimumBreak, maximumBreak)
		}

		return cases
	}
	
	
	/**
	 * Creates an array of non-overlapping possible results and picks 8
	 * @return Returns an array of 8 results for A1 that don't overlap
	 */
	private def genResults: Array[Int] = {
	  
		var minimumResult = this.minimumResult
		var maximumResult = this.maximumResult
		var minMaxDiff = maximumResult-minimumResult
		
		var arrayOfNumbers:Array[java.lang.Integer] = new Array[java.lang.Integer](minMaxDiff+1)

		
		for ( i <- 0 to minMaxDiff) {

		  arrayOfNumbers(i) = minimumResult
			minimumResult = minimumResult + 1
		}

		java.util.Collections.shuffle(java.util.Arrays.asList(arrayOfNumbers:_*)) // Using Collections to shuffle
		var arrayResults:Array[Int] = new Array[Int](8)
		
		for ( i <- 0 to 7) {
		  
			arrayResults(i) = arrayOfNumbers(i) // takes the first 8 from the shuffled array
		}
		
		return arrayResults
	}

		
	/**
	 * Generates a random number between two numbers
	 * @param min	the lower bound  
	 * @param max	the upper bound
	 * @return 		a random number
	 */
	private def randInt(min:Int, max:Int): Int = {
		
		val rand = new scala.util.Random
		var randomNum:Int = rand.nextInt((max - min) + 1) + min
		return randomNum
	}

	
	/**
	 * Used for generating random numbers within a range for the A1Test file
	 * @param number The breakpoint. 
	 * @param greaterThan Determines whether the number generated should be greater or less than.
	 * @return A random number
	 */
	private def randomNumForTests(number:Int, greaterThan:Int): Int = {

		var max:Int = number // use this set if looking for less than or equal to number
		var min:Int = minResultRange

		if (greaterThan == 1) { // if we want greater than number
			max = maxResultRange
			min = number + 1 // adding 1 so we get greater than (and not equal to)
		}
		
		return randInt(min,max)
	}

	
	/**
	 * @param studentNumber	a student number from file students.txt
	 * @param bp			array of breakpoints from genBreakpoints
	 * @param arrayResults	array of results from genResults
	 */
	private def outputA1Tests(studentNumber:String, bp:Array[Int], arrayResults:Array[Int]) {

		// Make directories
		var fname:String = "Assignment1/" + studentNumber + "/A1Test.java"
		val dir = new File(fname)
		dir.getParentFile.mkdirs

		// Initialize writers
		val fileWriter = new FileWriter(fname)
		val printWriter = new PrintWriter(fileWriter)
		printWriter.print("import static org.junit.Assert.*;\n\nimport org.junit.BeforeClass;\n"
							+ "import org.junit.Test;\n\nimport se2s03.A1;\n\npublic class A1Test {\n"
							+ "private static A1 tester;\n\n    @BeforeClass\n    public static void setUp(){\n"
							+ "    tester = new A1();\n    }\n\n    @Test\n    public void casesTest() {\n")


		// 		  0 -- elements of the branches array
		// 	    /   \
		//    1       2
		//   / \     / \
		//   3 4     5  6 -- 7 breakpoints (0-6)
		//  /\ /\   /\  /\ -- 8 branch options (paths to get to each result)
		// a b c d  e f g h -- 8 results
		
		var branches = Array.ofDim[Int](8,3)
		var branchOptions = Array.ofDim[Int](8,3)
		
		branches = Array( Array(bp(0),bp(1),bp(2)), Array(bp(0),bp(1),bp(2)), Array(bp(0),bp(1),bp(3)), 
		    Array(bp(0),bp(1),bp(3)), Array(bp(0),bp(4),bp(5)), Array(bp(0),bp(4),bp(5)), Array(bp(0),bp(4),bp(6)), Array(bp(0),bp(4),bp(6)) )
											// path to each result
											// there is only one point on top of the tree, bp(0), so I could have left it out (but I didn't)
											
		branchOptions = Array( Array( 0, 0, 0 ), Array( 0, 0, 1 ), Array( 0, 1, 0 ), Array( 0, 1, 1 ), Array( 1, 0, 0 ), Array( 1, 0, 1 ),
											Array( 1, 1, 0 ), Array( 1, 1, 1 ) )
											// all the left or right states of the paths in order (relative to branches and results)
											// 0 means lessthan (left); 1 means greaterthan (right)
		
		var numberOfCases:Int = this.numberOfCases // How many JUnit tests to create for each result.

		while (numberOfCases > 0) {
			
			for ( i <- 0 to branches.length - 1) {
				    
				var u = randomNumForTests(branches(i)(0), branchOptions(i)(0))
				var w = randomNumForTests(branches(i)(1), branchOptions(i)(1))
				var v = randomNumForTests(branches(i)(2), branchOptions(i)(2))
				var r = arrayResults(i)
				
				printWriter.println("        assertEquals(\"cases(" + u + ","
						+ w + "," + v + ") must be " + r + "\", " + r
						+ ", tester.cases(" + u + "," + w + "," + v + "));")
			}
			
			numberOfCases = numberOfCases - 1
		}
		
		printWriter.println("    }\n}")
		printWriter.close

		outputA1Java(studentNumber, bp, arrayResults)
	}

	
	private def outputA1Java(studentNumber:String, bp:Array[Int], arrayResults:Array[Int]) { //void

		// Make directories
		var fname = "Assignment1/" + studentNumber + "/se2s03/A1.java"
		val dir = new File(fname)
		dir.getParentFile.mkdirs

		// Initialize writers
		val fileWriter = new FileWriter(fname)
		val printWriter = new PrintWriter(fileWriter)

		printWriter.print("package se2s03;\n\npublic class A1 {\n    public int cases(int v, int u, int w) {\n"
				+ "        if (v <= " + bp(0) + ") {\n            if (u <= " + bp(1)
				+ ") {\n                if (w <= " + bp(2) + ") {\n                    return " + arrayResults(0)
				+ ";\n                } else {\n                    return " + arrayResults(1) + ";\n                }\n"
				+ "            } else {\n                if (w <= "	+ bp(3) + ") {\n                    return "
				+ arrayResults(2) + ";\n                } else {\n                    return "
				+ arrayResults(3) + ";\n                }\n            }\n        } else {\n            if (u <= " + bp(4)
				+ ") {\n                if (w <= " + bp(5) + ") {\n                    return " + arrayResults(4)
				+ ";\n                } else {\n                    return " + arrayResults(5) + ";\n                }\n"
				+ "            } else {\n                if (w <= "	+ bp(6) + ") {\n                    return "
				+ arrayResults(6) + ";\n                } else {\n                    return " + arrayResults(7)
				+ ";\n                }\n            }\n        }\n    }\n}")
		printWriter.close()
	}


	/**
	 * @return array of values/answers for A2
	 */
	private def genNumbersForA2: Array[Int] = {
		
		var cases:Array[Int] = Array( 0, 0, 0, 0, 0 )
		
		for ( i <- 0 to cases.length - 1 ) {
			
			cases(i) = randInt(-4, 6)
		}
		
		return cases
	}
	

	/**
	 * @param studentNumber	student number
	 * @param genNumbers 	array of answer values
	 */
	private def outputA2Tests(studentNumber:String, genNumbers:Array[Int]) { //void
		
		// Make directories
		var fname:String = "Assignment1/" + studentNumber + "/A2Test.java"
		val dir = new File(fname)
		dir.getParentFile.mkdirs

		// Initialize writers
		val fileWriter = new FileWriter(fname)
		val printWriter = new PrintWriter(fileWriter)
		printWriter.print("import static org.junit.Assert.*;\n\nimport org.junit.BeforeClass;\nimport org.junit.Test;\n\n"
				+"import se2s03.A2;\n\npublic class A2Test {\n    private static A2 tester;\n\n    @BeforeClass\n"
				+"    public static void setUp(){\n        tester = new A2();\n    }\n\n    @Test\n    public void RecTest() {\n")
	
		for ( i <- 0 to 29 ) {
			var r = Rec(genNumbers, i)
			printWriter.println("        assertEquals(\"Rec("+i+") must be "+r+"\", "+r+", tester.Rec("+i+"));")
		}
		
		printWriter.println("    }\n}")
		printWriter.close
		
		outputA2Java(studentNumber, genNumbers)
	}
	
	
	/**
	 * @param studentNumber	student number
	 * @param gen			array of answer values
	 */
	private def outputA2Java(studentNumber:String, gen:Array[Int]) { //void
		
		// Make directories
		var fname = "Assignment1/" + studentNumber + "/se2s03/A2.java"
		val dir = new File(fname)
		dir.getParentFile.mkdirs

		// Initialize writers
		val fileWriter = new FileWriter(fname)
		val printWriter = new PrintWriter(fileWriter)
		
		printWriter.print("package se2s03;\n\npublic class A2 {\n    public int Rec(int n) {\n"
				+"        int a0 = "+gen(0)+", a1 = "+gen(1)+", an = "+gen(2)+" ;\n        int x = "+gen(3)+", y = "+gen(4)+" ;\n\n"    
				+"        if ( n == 0 ) return a0;\n        if ( n == 1 ) return a1;\n\n"
				+"        for ( int i=2; i <= n; i++ ) {\n            an= x * a0 + y * a1;\n"
				+"            a0 = a1;\n            a1 = an;\n        }\n        return an;\n    }\n}\n")
		printWriter.close
		
	}
	
	
    /**
     * @param gen	The randomized values/answers
     * @param n		parameter of the function
     * @return 		The answer given relative to n
     */
    def Rec( gen:Array[Int], n:Int ): Int = {
    	
        var a0:Int = gen(0)
        var a1:Int = gen(1)
        var an:Int = gen(2)
        var x:Int = gen(3)
        var y:Int = gen(4)
        
        if ( n == 0 ) return a0
        if ( n == 1 ) return a1
        
        for ( i <- 2 to n) {
            an= x * a0 + y * a1 
            a0 = a1
            a1 = an
        }
        return an
    }
}