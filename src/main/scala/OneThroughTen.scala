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
    val max = math.pow(10, digits) - 1
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

  //   println(largestPalindromeProduct(3))

  // Problem 5
  def smallestMultiple(n: Int) = {
    val input = (1 to n).toList
    def exists(n: Int, list: List[Int]): Boolean = {
      if (list.isEmpty) false
      else if (n == 1) true
      else if (n % list.head == 0) exists(n / list.head, list.tail)
      else exists(n, list.tail)
    }

    def getProduct(n: List[Int], acc: Int): Int = {
      if (n.isEmpty) acc
      else if (acc % n.head == 0) getProduct(n.tail, acc)
      else getProduct(n.tail, acc * n.head)
    }

    def helper(input: List[Int], acc: List[Int]): List[Int] = {
      if (input.isEmpty) acc
      else if (exists(input.head, acc)) helper(input.tail, acc)
      else helper(input.tail, input.head :: acc)
    }

    getProduct(helper(input, List()), 1)
  }

  //  println(smallestMultiple(20))

  // Problem 6
  def sumSquareDifference(n: Int): Unit = {
    // use math!
  }

  //  println(sumSquareDifference(100))

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
  def largestMultipleOf13AdjacentDigits(n: String): BigInt = {
    def splitInto13(x: String, acc: List[String]): List[String] = {
      if (x.length < 13) acc
      else splitInto13(x.substring(1), x.substring(0, 13) :: acc)
    }

    val result = for {
      x <- n.replace("\n", "").split("0") // get each number on split of zero
      y <- splitInto13(x, List()) // get at list of 13-length string
      t = y.toList.map(s => Integer.parseInt(s.toString)) // for each 13-length string, convert to a list of 13 digits
    } yield t.foldRight(BigInt(1))((s, acc) => BigInt(s) * acc) // fold right to get the product

    result.max
  }

  //  println(
  //    largestMultipleOf13AdjacentDigits(
  //      "73167176531330624919225119674426574742355349194934\n" +
  //        "96983520312774506326239578318016984801869478851843\n" +
  //        "85861560789112949495459501737958331952853208805511\n" +
  //        "12540698747158523863050715693290963295227443043557\n" +
  //        "66896648950445244523161731856403098711121722383113\n" +
  //        "62229893423380308135336276614282806444486645238749\n" +
  //        "30358907296290491560440772390713810515859307960866\n" +
  //        "70172427121883998797908792274921901699720888093776\n" +
  //        "65727333001053367881220235421809751254540594752243\n" +
  //        "52584907711670556013604839586446706324415722155397\n" +
  //        "53697817977846174064955149290862569321978468622482\n" +
  //        "83972241375657056057490261407972968652414535100474\n" +
  //        "82166370484403199890008895243450658541227588666881\n" +
  //        "16427171479924442928230863465674813919123162824586\n" +
  //        "17866458359124566529476545682848912883142607690042\n" +
  //        "24219022671055626321111109370544217506941658960408\n" +
  //        "07198403850962455444362981230987879927244284909188\n" +
  //        "84580156166097919133875499200524063689912560717606\n" +
  //        "05886116467109405077541002256983155200055935729725\n" +
  //        "71636269561882670428252483600823257530420752963450")
  //  )


  // Problem 9
  def findPythagoreanTriplet(sum: Int) = {
    for {
      a <- Range(1, sum / 3)
      b <- Range(1, sum / 2)
      c = 1000 - a - b
      if a * a + b * b == c * c
    } yield a * b * c
  }

  //  println(findPythagoreanTriplet(1000))
  // Problem 10
  def isPrime1(n: Int) = {
    def isPrimeHelper(n: Int, min: Int, max: Int): Boolean = {
      if (n == 2) true
      else if (n == 3) true
      else if (min >= n) true
      else if (n % min == 0) false
      else isPrimeHelper(n, min + 2, max)
    }
    isPrimeHelper(n, 3, math.sqrt(n).toInt)
  }

  def sumPrimes(max: Long) = {
    def sumHelper(current: Int, max: Long, acc: Long): Long = {
      if (current >= max) acc
      else if (isPrime1(current)) sumHelper(current + 2, max, acc + current)
      else sumHelper(current + 2, max, acc)
    }
    sumHelper(3, max, 0) + 2
  }

  println(sumPrimes(2000000))
}
