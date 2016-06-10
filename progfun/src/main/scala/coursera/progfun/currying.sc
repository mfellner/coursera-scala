def sum(f: Int => Int): (Int, Int) => Int = {
  def sumF(a: Int, b: Int): Int =
    if (a > b) 0
    else f(a) + sumF(a + 1, b)
  sumF
}

def sumInts = sum(x => x)
def sumCubes = sum(x => x * x * x)

sumInts(1, 4)
1 + 2 + 3 + 4

def sum2(f: Int => Int)(a: Int, b: Int): Int =
  if (a > b) 0 else f(a) + sum(f)(a + 1, b)

sum2(identity)(1, 4)

def product(f: Int => Int)(a: Int, b: Int): Int =
  if (a > b) 1 else f(a) * product(f)(a + 1, b)

def factorial(n: Int) = product(identity)(1, n)
factorial(4)
1 * 2 * 3 * 4

def fold(e: Int, g: (Int, Int) => Int)(f: Int => Int)(a: Int, b: Int): Int =
  if (a > b) e else g(f(a), fold(e, g)(f)(a + 1, b))

fold(0, (a, b) => a + b)(identity)(1, 4)
1 + 2 + 3 + 4

def sum3 = fold(0, (a, b) => a + b) _
sum3(identity)(1, 4)

def product2 = fold(1, (a, b) => a * b) _
def factorial2(n: Int) = product2(identity)(1, n)
factorial2(4)
1 * 2 * 3 * 4
