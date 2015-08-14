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

  // Problem 7
  def nextPrime(currentPrimes: List[Int]) = {
    def isPrime(n: Int): Boolean = (for (x <- currentPrimes.reverse if n % x == 0) yield x).isEmpty

    def getPrimeAfter(n: Int): Int = {
      if (isPrime(n + 2)) n + 2
      else getPrimeAfter(n + 2)
    }

    if (currentPrimes.isEmpty) List(3, 2)
    else getPrimeAfter(currentPrimes.head) :: currentPrimes
  }

  def nthPrime(n: Int, acc: List[Int]): List[Int] = {
    if (n == 1) acc
    else nthPrime(n - 1, nextPrime(acc))
  }

  println(nthPrime(10001, List()).head)

}
