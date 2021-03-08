object Module {
    sealed trait List[+A]
    case object Nil extends List[Nothing]
    case class Cons[+A](head: A, tail: List[A]) extends List[A]
    object List {
        def sum(ints: List[Int]): Int = ints match {
            case Nil => 0
            case Cons(x,xs) => x + sum(xs)
        }

        def product(ds: List[Double]): Double = ds match {
            case Nil => 1.0
            case Cons(0.0, _) => 0.0
            case Cons(x,xs) => x * product(xs)
        }

        def apply[A](as: A*): List[A] =
            if (as.isEmpty) Nil
            else Cons(as.head, apply(as.tail: _*))

        // Exercise 3.2
        def tail[A](as: List[A]): List[A] = as match {
            case Nil => Nil
            case Cons(h, t) => t
        }

        // Exercise 3.3
        def setHead[A](a: A, as: List[A]): List[A] = as match {
            case Nil => Cons(a, Nil)
            case Cons(h, t) => Cons(a, t)
        }

        // Exercise 3.4
        def drop[A](l: List[A], n: Int): List[A] = l match {
            case Nil => Nil
            case Cons(h, t) =>
                if (n <= 0) Cons(h, t)
                else drop(t, n-1)
        }

        // Exercise 3.5
        def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
            case Nil => Nil
            case Cons(h, t) =>
                if (f(h)) dropWhile(t, f)
                else Cons(h, t)
        }

        // Exercise 3.6
        def init[A](l: List[A]): List[A] = l match {
            case Nil => Nil
            case Cons(a, Nil) => Nil
            case Cons(a, Cons(b, c)) => Cons(a, Cons(b, Nil))
            case Cons(a, b) => Cons(a, init(b))
        }

        // Exercise 3.7
        def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = as match {
            case Nil => z
            case Cons(x, xs) => f(x, foldRight(xs, z)(f))
        }

        // Exercise 3.9
        def length[A](as: List[A]): Int =
            foldRight(as, 0)((x, y) => (1 + y))

        // Exercise 3.10
        def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
            case Nil => z
            case Cons(x, xs) => f(foldLeft(xs, z)(f), x)
        }

        // Exercise 3.11
        def sum2(ints: List[Int]): Int =
            foldLeft(ints, 0)(_ + _)

        def product2(doubles: List[Double]): Double =
            foldLeft(doubles, 1.0)(_ * _)

        def length2[A](as: List[A]): Int =
            foldLeft(as, 0)((x, y) => (1 + x))

        // Exercise 3.12
        def reverse[A](as: List[A]): List[A] = as match {
            case Nil => Nil
            case Cons(x, xs) => Cons(reverse(xs), Cons(x, Nil))
        }
    }

    // Exercise 3.1; will return 3 (matches 3rd case)
    val x = List(1,2,3,4,5) match {
        case Cons(x, Cons(2, Cons(4, _))) => x
        case Nil => 42
        case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
        case Cons(h, t) => h + List.sum(t)
        case _ => 101
    }
}