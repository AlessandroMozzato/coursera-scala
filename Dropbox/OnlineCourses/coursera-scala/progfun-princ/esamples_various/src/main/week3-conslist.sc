import week3._

def nth(n: Int, xs: List[T]): T = {
  if (n == 0) xs.head
  else nth(n - 1, xs.tail)
}
