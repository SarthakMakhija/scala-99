# scala-99
Repository containing the solutions to problems defined in http://aperiodic.net/phil/scala/s-99/

The project includes - 
1. Scala
2. Scala test
3. Sbt 

Every exercise is grouped into sections "Working with lists" under a class WorkingWithLists and so on.

```
object WorkingWithLists {

  def nth(k: Int, xs: List[Int]) = {
    def nth$(itr: Int, xs: List[Int]): Int = xs match {
      case Nil => -1
      case head :: tail if itr == k => head
      case _ => nth$(itr + 1, xs.tail)
    }
    nth$(itr = 1, xs)
  }

```
and the tests for same -

```
@RunWith(classOf[JUnitRunner])
class WorkingWithListsTest extends FunSuite {

  test("should return kth elements list k = 3") {
    val xs = List(1, 2, 3, 4, 5)
    val kth = WorkingWithLists.nth(3, xs)

    assert(kth == 3)
  }
```

To run the tests -

```
sbt test
```
