def product(f: Int => Int)(a: Int, b: Int): Int = {
  def loop(a: Int, acc: Int): Int = {
    if (a>b) acc
    else loop(a+1,f(a)*acc)
  }
  loop(a,1)
}

def product1(f: Int => Int)(a: Int, b: Int): Int = {
  if (a>b) 1
  else f(a)*product(f)(a+1,b)
}

def sum(f: Int => Int)(a: Int, b: Int): Int = {
  if (a>b) 0
  else f(a)+sum(f)(a+1,b)
}

def fact(a:Int) = {product1(x=>x)(1,a)}

sum(x=>x)(2,5)
product1(x=>x*x)(2,5)
product(x=>x*x)(2,5)

fact(10)

def mapReduce(f: Int=> Int, combine: (Int,Int) => Int, zero: Int)(a:Int, b: Int): Int = {
  if (a>b) zero
  else combine(f(a),mapReduce(f,combine,zero))(a+1,b)
}

