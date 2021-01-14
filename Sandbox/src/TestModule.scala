object TestFuncs{

  def abs(x: Int):Int ={
    if(x<0) -x
    else x
  }

  def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d."
    msg.format(x,abs(x))
  }

  def factorial(n: Int):Int = {
    // Recursive Inner Function
    def go(n: Int, acc: Int): Int = {
        //Base case
      if (n <= 0) acc
        //Recurse
      else go(n - 1, n * acc)
    }
    //Kick it off with initial conditions
    go(n, 1)
  }

  def formatFactorial(n: Int) = {
    val msg = "The factorial of %d is %d"
    msg.format(n,factorial(n))
  }

  def fibonacci(n: Int): Int = {
    // 0 1 1 2 3 5 8 13 ... ect
    def loop(n:Int,curr :Int, next:Int): Int ={
      if (n==0) curr
      else loop(n-1, next, curr + next )
    }

    loop(n,0,1)
  }

  def formatFibonacci(n: Int) = {
    val msg = "The %d'th fibonacci number is %d"
    msg.format(n,fibonacci(n))
  }

  def formatResult(name: String, n: Int , f:Int=>Int)={
    val msg = " The %s of %d is %d"
    msg.format(name,n,f(n))
  }

  // Generic Function
  // This example is cool. 'as' is an array of generic objects of type 'T'
  // Generic and user defines the type to bool condition func that must be met
  // First that meets it has its index passed on
  def findFirst[T](as:Array[T],p: T=>Boolean): Int = {
    def loop(n:Int): Int ={
      if (n >= as.length) -1
      else if (p(as(n))) n
      else loop(n+1)
    }
    loop(0)
  }

  def isSorted[T] (as: Array[T], ordered: (T,T) => Boolean) : Boolean = {
    def loop(n:Int) : Boolean = {
        if (n>= as.length - 1) true
        else if (!ordered(as(n),as(n+1))) false
        else loop(n+1)
    }

    loop(0)
  }

  def main(args: Array[String]): Unit = {
    println(formatResult("Factorial", 6, factorial))
    println(formatResult("Fibonacci", 5, fibonacci))
  }
}
