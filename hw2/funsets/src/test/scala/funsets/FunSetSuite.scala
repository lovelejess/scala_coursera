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
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   * 
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(x) contains y") {
    
    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3". 
     */
    new TestSets {
      assert(contains(s1, 1), "Singleton (s1,1)")
      assert(contains(s2, 2), "Singleton (s2,2)")
      assert(contains(s3, 3), "Singleton (s3,3)")
    }
  }

  test("singletonSet Sx does not contain y"){
    new TestSets {
      assert(!contains(s1,2), "Singleton (s1,2)")
      assert(!contains(s2,1), "Singleton (s2,1)")
    }
  }

  test("union contains all elements") {
    new TestSets {
      val unionSet = union(s1, s2)
      assert(contains(unionSet, 1), "Union 1")
      assert(contains(unionSet, 2), "Union 2")
      assert(!contains(unionSet, 3), "Union 3")
    }
  }

  test("intersect contains only elements in both sets"){
    new TestSets{
      val intersectionSet = intersect(s1,s2)
      assert(!contains(intersectionSet, 1), "Intersection 1")
      assert(!contains(intersectionSet, 2), "Intersection 2")
    }
  }

  test("diff contains all the elements of the set s that are not in the set t"){
    new TestSets{
      val diffSets1s2 = diff(s1, s2)
      assert(contains(diffSets1s2,1), "diff 1 ")
      assert(!contains(diffSets1s2,2), "diff 2 ")

      val diffSets3s1 = diff(s3, s1)
      assert(contains(diffSets3s1, 3), "diff 3 ")
      assert(!contains(diffSets3s1, 1), "diff 1 ")
    }
  }
}
