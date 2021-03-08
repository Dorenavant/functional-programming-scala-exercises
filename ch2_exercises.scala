object Module {
    // Exercise 2.1
    def fibonacci(n: Int) = {
        @annotation.tailrec
        def go(n: Int, fib1: Int, fib2: Int): Int =
            if (n == 1) fib1
            else if (n == 2) fib2
            else go(n-1, fib2, fib1 + fib2)
            
        go(n, 0, 1)
    }

    // Exercise 2.2
    def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
        def go(n: Int): Boolean =
            if (n >= as.length - 1) true
            else if (ordered(as(n), as(n+1))) go(n+1)
            else false
        go(0)
    }

    // Exercise 2.3
    def curry[A, B, C](f: (A, B) => C): A => (B => C) =
        (a: A) => (b: B) => f(a, b)

    // Exercise 2.4
    def uncurry[A, B, C](f: A => B => C): (A, B) => C =
        (a: A, b: B) => f(a)(b)

    // Exercise 2.5
    def compose [A, B, C](f: B => C, g: A => B): A => C =
        (a: A) => f(g(a))
}