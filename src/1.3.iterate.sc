object item3 {
	
	// do iterating exponention
	// using when n is even > iter_expt(n/2)*iter_expt(n/2),
	// b^(n^(m)) = b^(m^(n)) : b^(n/2^(2)) = b^(2^(n/2))
	// otherwise (odd) > (iter_expt(n-1, b, a*b))
	
	def expt(n: Int, b: Int): Int = {
		def iter_expt(n: Int, b: Int, a: Int) : Int = {
			println("n: " + n + ", b: " + b + ", a: " + a);
			if (n == 0) 1
			else if (n == 1) b*a
			// for 2, need manual operating for avoiding infinite loop
			else if (n!=2 && n % 2 == 0) iter_expt(2, iter_expt(n/2, b, 1), a) // ?? is this tail recursion??
			else iter_expt(n-1, b, a*b)
		}
		if (n == 1) b
		else iter_expt(n, b, 1)
	}                                         //> expt: (n: Int, b: Int)Int
	expt(3, 4)                                //> n: 3, b: 4, a: 1
                                                  //| n: 2, b: 4, a: 4
                                                  //| n: 1, b: 4, a: 16
                                                  //| res0: Int = 64
}

/* 2^2 = 2^((1)^2) = 4
iter_expt(2, iter_expt(1, 2, 1), 1)
iter_expt(2, 2, 1)
*/