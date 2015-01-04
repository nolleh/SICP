object part2 {

  def countChange(money: Int) = {
    val change = Array(1, 5, 10, 25, 50)
    def iterChange(remain: Int, changeIndex: Int): Int = {
      if (remain == 0) 1
      else if (changeIndex == change.size) 0
      else if (remain < change(changeIndex)) 0
      else
        iterChange(remain, changeIndex + 1) +
          iterChange(remain - change(changeIndex), changeIndex)
    }
    iterChange(money, 0)
  }                                               //> countChange: (money: Int)Int
  countChange(100)                                //> res0: Int = 292


	//// ======2=========
	def recursive_f(n: Int): Int = {
		if (n < 3)	n
		else {
			recursive_f(n-1) + 2 * recursive_f(n-2) + 3 * recursive_f(n-3)
		}
	}                                         //> recursive_f: (n: Int)Int
	recursive_f(4)                            //> res1: Int = 11

	
	// f(4) = f(3) + 2f(2) + 3f(1) = 2 + 2 + 4 + 3 = 11
	def iterative_f(n: Int): Int = {
		def iter_f(a: Int, b: Int, c:Int, n: Int): Int = {
			if (n < 3) return a
		  var newFN = a + 2*b + 3*c
			iter_f(newFN, a, b, n-1)
		}
		if (n < 3) n
		else iter_f(2,1,0,n)
	}                                         //> iterative_f: (n: Int)Int
	iterative_f(4)                            //> res2: Int = 11

  def pascal(l: Int): Unit = {
  	def pascalIter(i: Int, l: Int): Unit = {
	    def pascalElement(r: Int, c: Int): Int = {
	      if (c == 0 || c == r) return 1
	      return pascalElement(r - 1, c - 1) + pascalElement(r - 1, c)
	    } //pascalElem
	    
	    if (i != l) {
	    	// not 1/2 <= already contains space for each number
	    	for (x <- 0 until l - i)
	    		print(" ")
	    	// pascal element start r = 0 / c = 0
	    	for (c <- 0 until i+1) { // for when i is 0, want to start pascal. so add 1
	        print(pascalElement(i, c) + " ")
	      }
	      println()
	      pascalIter(i+1, l)
	    }
    }
    // line number start from 0
    pascalIter(0, l)
  }                                               //> pascal: (l: Int)Unit
  pascal(7)                                       //>        1 
                                                  //|       1 1 
                                                  //|      1 2 1 
                                                  //|     1 3 3 1 
                                                  //|    1 4 6 4 1 
                                                  //|   1 5 10 10 5 1 
                                                  //|  1 6 15 20 15 6 1 
}