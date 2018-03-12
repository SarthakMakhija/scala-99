object WorkingWithLists {

  def last(xs: List[Int]) = if (xs.isEmpty) -1 else xs.last

  def penultimate(xs: List[Int]) = if (xs.isEmpty) -1 else xs.init.last

  def nth(k: Int, xs: List[Int]) = {
    def nth$(itr: Int, xs: List[Int]): Int = xs match {
      case Nil => -1
      case head :: tail if itr == k => head
      case _ => nth$(itr + 1, xs.tail)
    }
    nth$(itr = 1, xs)
  }

  def length(xs: List[Int]) = xs.size

  def reverse(xs: List[Int]) = xs.reverse

  def isPalindrome(xs: List[Int]) = {
    def isPalindrome$(xs: List[Int], ys: List[Int]): Boolean = (xs, ys) match {
      case (Nil, _) => false
      case (_, Nil) => false
      case (hx :: tx, hy :: ty) if tx.isEmpty && ty.isEmpty && hx == hy => true
      case (hx :: tx, hy :: ty) if hx != ty.last => false
      case _ => isPalindrome$(xs.tail, ys.init)
    }
    isPalindrome$(xs, xs)
  }

  def duplicate(xs: List[Char]): List[Char] = duplicateN(2, xs)

  def duplicateN(n: Int, xs: List[Char]): List[Char] = {
    def duplicateN$(xs: List[Char], acc: List[Char]): List[Char] = xs match {
      case Nil => acc
      case head :: tail => duplicateN$(tail, acc ::: List.fill(n)(head))
    }
    duplicateN$(xs, List())
  }

  def drop(n: Int, xs: List[Char]): List[Char] = {
    def drop$(iter: Int, xs: List[Char], acc: List[Char]): List[Char] = xs match {
      case Nil => acc
      case head :: tail if iter == n => drop$(1, tail, acc)
      case head :: tail if iter != n => drop$(iter + 1, tail, acc ::: List(head))
    }
    drop$(1, xs, List())
  }

  def split(at: Int, xs: List[Char]): (List[Char], List[Char]) = {
    def split$(iter: Int, xs: List[Char], acc: List[Char]): (List[Char], List[Char]) = xs match {
      case Nil => Tuple2(List(), List())
      case head :: tail if iter == at => (acc ::: List(head), tail)
      case head :: tail if iter != at => split$(iter + 1, tail, acc ::: List(head))
    }
    split$(1, xs, List())
  }
}