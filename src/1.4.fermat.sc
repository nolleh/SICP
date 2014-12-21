import util.Random

object item4 {

	def expmod(base:Int, exp:Int, m:Int): Int = {
		def square(a:Int): Int = {
			a * a
		}
		if (exp == 0) 1
		else if (exp % 2 == 0) (square(expmod(base, exp/2, m)) % m)
		else (base * (expmod(base, exp-1, m))) % m
	}                                         //> expmod: (base: Int, exp: Int, m: Int)Int
	// is this number congruent with any a?
	def fermat_test(n: Int): Boolean = {
		def try_it(a: Int): Boolean = {
			expmod(a, n, n) == a // it is really do check with a...
		}
		try_it(Random.nextInt(n-1) + 1)
	}                                         //> fermat_test: (n: Int)Boolean
	
	// try test with given number
	def fast_prime(n: Int, times: Int): Boolean = {
		if (times == 0) true
		else if (fermat_test(n)) fast_prime(n, times-1)
		else false
	}                                         //> fast_prime: (n: Int, times: Int)Boolean
	
	// -----
	// exercise 1.27
	fermat_test(13)                           //> res0: Boolean = true
	fermat_test(561)                          //> res1: Boolean = true
	fermat_test(1105)                         //> res2: Boolean = true
	fermat_test(2465)                         //> res3: Boolean = true
	
	
	// exercise 1.28 - miller-Rabin test
	def millerRabin(n: Int): Boolean = {
		def square(a: Int): Int = a*a
		
		def try_it(a: Int): Boolean = {
			expmod(a, n-1, n) == 1
		}
		try_it(Random.nextInt(n-1) + 1)
	}                                         //> millerRabin: (n: Int)Boolean
	millerRabin(12)                           //> res4: Boolean = false
	millerRabin(13)                           //> res5: Boolean = true
	millerRabin(1105)                         //> res6: Boolean = true
}