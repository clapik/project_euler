object OneThroughTen extends App {

  // Problem 1
  def sum(start: Int, end: Int, acc: Int): Int = {
    if (start == end) acc
    else if (start % 3 == 0) sum(start + 1, end, acc + start)
    else if (start % 5 == 0) sum(start + 1, end, acc + start)
    else sum(start + 1, end, acc)
  }

  //  println(sum(1, 1000, 0))

  // Problem 2
  def nextEvenFib(nMinus2: Int, nMinus1: Int): (Int, Int, Int) = {
    val n = nMinus1 + nMinus2
    if (n % 2 == 0) (nMinus2, nMinus1, n)
    else nextEvenFib(nMinus1, n)
  }

  def sumEvenFib(nMinus2: Int, nMinus1: Int, max: Int, acc: Int): Int = {
    val n = nextEvenFib(nMinus2, nMinus1)
    if (n._3 > max) acc
    else sumEvenFib(n._2, n._3, max, acc + n._3)
  }

  //  println(sumEvenFib(0, 1, 4000000, 0))

  // Problem 3
  def largestPrimeFactorOf(n: Long): Long = {
    def primeFactors(n: Long): List[Long] = {
      def helper(n: Long, primes: List[Long]): List[Long] = {
        if (n <= primes.head) primes
        else if (n % primes.head == 0) {
          helper(n / primes.head, primes)
        } else helper(n, nextPrime(primes))
      }

      helper(n, nextPrime(List()))
    }
    primeFactors(n).head
  }

  // println(largestPrimeFactorOf(600851475143L))

  // Problem 4
  def largestPalindromeProduct(digits: Int): Int = {
    val min = math.pow(10, digits - 1).toInt
    val max = math.pow(10, digits) - 1.toInt
    def isPalindrome(s: String): Boolean = {
      if (s.length <= 1) true
      else if (s.charAt(0) == s.charAt(s.length - 1)) isPalindrome(s.substring(1, s.length - 1))
      else false
    }

    def helper(x: Int, y: Int, largestSoFar: Int): Int = {
      val prod = x * y
      if (x > max) largestSoFar
      else if (y > max) {
        if (isPalindrome(prod.toString) && prod > largestSoFar) helper(x + 1, min, prod)
        else helper(x + 1, min, largestSoFar)
      }
      else {
        if (isPalindrome(prod.toString) && prod > largestSoFar) helper(x, y + 1, prod)
        else helper(x, y + 1, largestSoFar)
      }
    }

    helper(min, min, 0)
  }

  // println(largestPalindromeProduct(3))

  // Problem 5
  def smallestMultiple(n: Int) = {
    val input = (1 to n).toList
    def exists(n: Int, list: List[Int]): Boolean = {
      if(list.isEmpty) false
      else if(n == 1) true
      else if(n % list.head == 0) exists(n / list.head, list.tail)
      else exists(n, list.tail)
    }

    def getProduct(n: List[Int], acc: Int): Int = {
      if(n.isEmpty) acc
      else if(acc % n.head == 0) getProduct(n.tail, acc)
      else getProduct(n.tail, acc * n.head)
    }

    def helper(input: List[Int], acc: List[Int]): List[Int] = {
      if(input.isEmpty) acc
      else if(exists(input.head, acc)) helper(input.tail, acc)
      else helper(input.tail, input.head :: acc)
    }

    getProduct(helper(input, List()), 1)
  }

//  println(smallestMultiple(20))

  // Problem 6
  def sumSquareDifference(n:Int): Unit = {
    // use math!
  }

  println(sumSquareDifference(100))
  // Problem 7
  def nextPrime(currentPrimes: List[Long]) = {
    def isPrime(n: Long): Boolean = (for (x <- currentPrimes.reverse if n % x == 0) yield x).isEmpty

    def getPrimeAfter(n: Long): Long = {
      if (isPrime(n + 2)) n + 2
      else getPrimeAfter(n + 2)
    }

    if (currentPrimes.isEmpty) List(3L, 2L)
    else getPrimeAfter(currentPrimes.head) :: currentPrimes
  }

  def nthPrime(n: Long) = {
    def helper(n: Long, acc: List[Long]): List[Long] = {
      if (n == 1) acc
      else helper(n - 1, nextPrime(acc))
    }

    helper(n, List()).head
  }


  //  println(nthPrime(10001L, List()).head)

  // Problem 8

  // Problem 9

  // Problem 10

}
