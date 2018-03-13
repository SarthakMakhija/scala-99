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

  def slice(startIncluding: Int, endExcluding: Int, xs: List[Char]): List[Char] = {
    def slice$(iter: Int, xs: List[Char], acc: List[Char]): List[Char] = xs match {
      case Nil => acc
      case head :: tail if iter >= startIncluding && iter < endExcluding => slice$(iter + 1, tail, acc ::: List(head))
      case _ => slice$(iter + 1, xs.tail, acc)
    }
    slice$(0, xs, List())
  }

  def rotate(by: Int, xs: List[Char]): List[Char] = ???

  def removeAt(index: Int, xs: List[Char]): (List[Char], Char) = {
    def removeAt$(iter: Int, xs: List[Char], acc: List[Char], removed: Char): (List[Char], Char) = xs match {
      case Nil => (acc, removed)
      case head :: tail if iter == index => removeAt$(iter + 1, tail, acc, head)
      case _ => removeAt$(iter + 1, xs.tail, acc ::: List(xs.head), removed)
    }
    removeAt$(0, xs, List(), '\0')
  }

  def insertAt(value: Char, index: Int, xs: List[Char]): List[Char] = {
    def insertAt$(iter: Int, xs: List[Char], acc: List[Char]): List[Char] = xs match {
      case Nil => acc
      case head :: tail if iter == index => insertAt$(iter + 1, tail, acc ::: List(value, head))
      case _ => insertAt$(iter + 1, xs.tail, acc ::: List(xs.head))
    }
    insertAt$(0, xs, List())
  }

  def range(start: Int, end: Int): List[Int] = {
    def range$(iter: Int, acc: List[Int]): List[Int] = {
      if (iter <= end)
        range$(iter + 1, acc ::: List(iter))
      else
        acc
    }
    range$(start, List())
  }


}