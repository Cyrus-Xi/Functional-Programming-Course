package funsets

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {


  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  test("string take") {
    val message = "hello, world"
    assert(message.take(5) == "hello")
  }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  test("adding ints") {
    assert(1 + 2 === 3)
  }

  
  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }
  
  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   * 
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   * 
   *   val s1 = singletonSet(1)
   * 
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   * 
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   * 
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    val s4 = singletonSet(3)
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   * 
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {
    
    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3". 
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
    }
  }

  test("union contains all elements") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }
  
  test("intersection contains shared elements") {
    new TestSets {
      val t1 = intersect(s1, s2)
      assert(!contains(t1, 1), "Intersect 1")
      val t2 = intersect(s3, s4)
      assert(contains(t2, 3), "Intersect 2")
      val t3 = intersect(union(s1, s2), s1)
      assert(contains(t3, 1), "Intersect 3")
    }
  }
  
  test("diff contains elements in first set but not in second") {
    new TestSets {
      val s = diff(s1, s2)
      val t = diff(s3, s4)
      val t2 = diff(union(s1, s2), s2)
      assert(contains(s, 1), "Diff 1")
      assert(!contains(s, 2), "Diff 2")
      assert(!contains(t, 3), "Diff 3")
      assert(contains(t2, 1), "Diff 4")
      assert(!contains(t2, 2), "Diff 4")
    }
  }
  
  test("filter returns the subset for which the predicate holds") {
    new TestSets {
      // {1, 2}
      val s = union(s1, s2)
      // {1, 2, 3}
      val t = union(s, s3)
      val less3 = (x: Int) => (x < 3)
      val ans = filter(s, less3)
      val ans2 = filter(t, less3)
      assert(ans(2), "Filter 1")
      assert(ans(1), "Filter 2")
      assert(!ans(3), "Filter 3")
      assert(ans2(2), "Filter 4")
      assert(ans2(1), "Filter 5")
      assert(!ans2(3), "Filter 6")
    }
  }
  
  test("forall returns whether all bounded integers in set satisfy predicate") {
    new TestSets {
      // {1, 2}
      val s = union(s1, s2)
      // {1, 2, 3}
      val t = union(s, s3)
      // The predicates.
      val less3 = (x: Int) => (x < 3)
      val great0 = (x: Int) => (x > 0)
      assert(forall(s, less3), "Forall 1")
      assert(forall(s, great0), "Forall 2")
      assert(!forall(t, less3), "Forall 3")
      assert(forall(t, great0), "Forall 4")
    }
  }
  
  test("exists returns whether there exists some x in set that satisfies predicate") {
    new TestSets {
      // {1, 2}
      val s = union(s1, s2)
      // {1, 2, 3}
      val t = union(s, s3)
      // The predicates.
      val less3 = (x: Int) => (x < 3)
      val great0 = (x: Int) => (x > 0)
      val great3 = (x: Int) => (x > 3)
      assert(exists(s, less3), "Exists 1")
      assert(exists(s, great0), "Exists 2")
      assert(!exists(s, great3), "Exists 3")
      assert(exists(t, less3), "Exists 4")
      assert(exists(t, great0), "Exists 5")
      assert(!exists(t, great3), "Exists 6")
    }
  }

  test("map returns a set transformed by applying a function to each x in s") {
    new TestSets {
      // {1, 2}
      val s = union(s1, s2)
      // {1, 2, 3}
      val t = union(s, s3)
      // The function.
      val plus1 = (x: Int) => (x + 1)
      val sPlus1 = map(s, plus1)
      val tPlus1 = map(t, plus1)
      assert(sPlus1(3), "Map 1")
      assert(!sPlus1(4), "Map 2")
      assert(tPlus1(4), "Map 3")
      assert(!tPlus1(5), "Map 3")
    }
  }
}
