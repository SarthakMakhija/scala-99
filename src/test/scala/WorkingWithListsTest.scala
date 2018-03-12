import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class WorkingWithListsTest extends FunSuite {

  test("should return the last element of a list") {
    val xs = List(1, 2, 3, 4, 5)
    val last = WorkingWithLists.last(xs)

    assert(last == 5)
  }

  test("should return -1 for last on empty list") {
    val xs = List()
    val last = WorkingWithLists.last(xs)

    assert(last == -1)
  }

  test("should return the second last element of a list") {
    val xs = List(1, 2, 3, 4, 5)
    val penultimate = WorkingWithLists.penultimate(xs)

    assert(penultimate == 4)
  }

  test("should return -1 for the second last on empty list") {
    val xs = List()
    val penultimate = WorkingWithLists.penultimate(xs)

    assert(penultimate == -1)
  }

  test("should return kth elements list k = 3") {
    val xs = List(1, 2, 3, 4, 5)
    val kth = WorkingWithLists.nth(3, xs)

    assert(kth == 3)
  }

  test("should return -1 for kth element on empty list") {
    val xs = List()
    val kth = WorkingWithLists.nth(3, xs)

    assert(kth == -1)
  }

  test("should return the length of the list") {
    val xs = List(1, 2, 3, 4, 5)
    val length = WorkingWithLists.length(xs)

    assert(length == 5)
  }

  test("should reverse the list") {
    val xs = List(1, 2, 3, 4, 5)
    val reversed = WorkingWithLists.reverse(xs)

    assert(reversed == List(5, 4, 3, 2, 1))
  }

  test("should return false if the list is not a palindrome") {
    val xs = List(1, 2, 3, 4, 5)
    val isPalindrome = WorkingWithLists.isPalindrome(xs)

    assert(!isPalindrome)
  }

  test("should return true if the list is a palindrome - I") {
    val xs = List(1, 2, 3, 2, 1)
    val isPalindrome = WorkingWithLists.isPalindrome(xs)

    assert(isPalindrome)
  }

  test("should return true if the list is a palindrome - II") {
    val xs = List(1, 2, 3, 4, 4, 3, 2, 1)
    val isPalindrome = WorkingWithLists.isPalindrome(xs)

    assert(isPalindrome)
  }

  test("should duplicate each element"){
    val xs = List('a', 'b', 'c', 'd', 'e')
    val duplicated = WorkingWithLists.duplicate(xs)

    assert(duplicated == List('a', 'a', 'b', 'b', 'c', 'c', 'd', 'd', 'e', 'e'))
  }

  test("should duplicate each element N times"){
    val xs = List('a', 'b', 'c', 'd', 'e')
    val duplicated = WorkingWithLists.duplicateN(3, xs)

    assert(duplicated == List('a', 'a', 'a', 'b', 'b', 'b', 'c', 'c', 'c', 'd', 'd', 'd', 'e', 'e', 'e'))
  }

  test("should drop every 3rd element"){
    val xs = List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k')
    val dropped = WorkingWithLists.drop(3, xs)

    assert(dropped == List('a', 'b', 'd', 'e', 'g', 'h', 'j', 'k'))
  }

  test("should drop every 1st element"){
    val xs = List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k')
    val dropped = WorkingWithLists.drop(1, xs)

    assert(dropped == List())
  }

  test("should split list at a given position"){
    val xs = List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k')
    val split = WorkingWithLists.split(3, xs)

    assert(split == (List('a', 'b', 'c'), List('d', 'e', 'f', 'g', 'h', 'i', 'j', 'k')))
  }
}
