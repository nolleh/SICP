import java.math;
object item1 {
  def examplePowSum(a: Int, b: Int, c: Int): Int = {
    def iter(x: Int, y: Int, last: Boolean): Int = {
      def pow(a: Int): Int = {
        a * a
      }
      if (last) if (x > y) x else y
      else if (x > y) pow(x) + pow(iter(y, c, true))
      else pow(y) + pow(c)
    }
    iter(a, b, false)
  }                                               //> examplePowSum: (a: Int, b: Int, c: Int)Int
  //examplePowSum(1, 3, 2)
  //examplePowSum(2, 1, 5)
  //examplePowSum(1, 4, 3)
	
	def newton = {
		def sqrt(x: Float) = {
			def iter_sqrt(guess: Float, x: Float): Float = {
				def improve(guess: Float): Float = {
					var avg = (x / guess + guess)/2
					println("guess: "+ avg )
					avg
				} // improve
				def good_enough(guess: Float):Boolean = {
					def pow(a: Float) = {
						a*a
					}
					if (Math.abs(pow(guess) - x) < 0.001) true else false
				}
				if (good_enough(guess))
					guess
				else{
					val new_guess = improve(guess)
					if (Math.abs(new_guess - guess) < guess * 0.1)
						-1
					else
						iter_sqrt(improve(guess), x)
				}
			} //iter_sqrt
			iter_sqrt(1, x)
		}
		sqrt(7000000000f)
	}                                         //> newton: => Float
	newton                                    //> guess: 3.5E9
                                                  //| guess: 3.5E9
                                                  //| guess: 1.75E9
                                                  //| guess: 1.75E9
                                                  //| guess: 8.75E8
                                                  //| guess: 8.75E8
                                                  //| guess: 4.375E8
                                                  //| guess: 4.375E8
                                                  //| guess: 2.18750016E8
                                                  //| guess: 2.18750016E8
                                                  //| guess: 1.09375024E8
                                                  //| guess: 1.09375024E8
                                                  //| guess: 5.4687544E7
                                                  //| guess: 5.4687544E7
                                                  //| guess: 2.7343836E7
                                                  //| guess: 2.7343836E7
                                                  //| guess: 1.3672046E7
                                                  //| guess: 1.3672046E7
                                                  //| guess: 6836279.0
                                                  //| guess: 6836279.0
                                                  //| guess: 3418651.5
                                                  //| guess: 3418651.5
                                                  //| guess: 1710349.5
                                                  //| guess: 1710349.5
                                                  //| guess: 857221.1
                                                  //| guess: 857221.1
                                                  //| guess: 432693.53
                                                  //| guess: 432693.53
                                                  //| guess: 224435.62
                                                  //| guess: 224435.62
                                                  //| guess: 127812.484
                                                  //| guess: 127812.484
                                                  //| guess: 91290.11
                                                  //| guess: 91290.11
                                                  //| guess: 83984.37
                                                  //| res0: Float = -1.0
  def newton3 = {
		def sqrt(x: Float) = {
			def iter_sqrt(guess: Float, x: Float): Float = {
				def improve(guess: Float): Float = {
					var avg = (x / (guess*guess) + 2*guess)/3
					println("guess : " + avg)
					avg
				} // improve
				def good_enough(guess: Float):Boolean = {
					def pow(a: Float) = {
						a*a*a
					}
					if (Math.abs(pow(guess) - x) < 0.001) true else false
				}
				if (good_enough(guess))
					guess
				else
					iter_sqrt(improve(guess), x)
			} //iter_sqrt
			iter_sqrt(1, x)
		}
		sqrt(10)
	}                                         //> newton3: => Float
	//newton3
}