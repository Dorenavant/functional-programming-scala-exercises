object Module {
    def fibonacci(n: Int) = {
        @annotation.tailrec
        def go(n: Int, fib1: Int, fib2: Int): Int = {
            if (n == 1) fib1
            else if (n == 2) fib2
            else go(n-1, fib2, fib1 + fib2)
        }
        go(n, 0, 1)
    }
}