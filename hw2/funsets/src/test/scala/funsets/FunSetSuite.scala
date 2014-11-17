package funsets

import org.scalacheck.Prop.True
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
  test("string take")
  {
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
  test("adding ints")
  {
    assert(1 + 2 === 3)
  }


  import FunSets._

  test("contains is implemented")
  {
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

  trait TestSets
  {
    val singletonSet1 = singletonSet(1)
    val singletonSet2 = singletonSet(2)
    val singletonSet3 = singletonSet(3)
    val singletonSet4 = singletonSet(4)
    val negativeIntegerSet = (x:Int) => x < 0
    val positiveIntegerSet = (x:Int) => x > 0
    val positiveSetXMod2 = (x:Int) => x % 2 > 0

  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(x) contains y")
  {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3". 
     */
    new TestSets
    {
      assert(contains(singletonSet1 , 1), "Singleton (s1,1)")
      assert(contains(singletonSet2, 2), "Singleton (s2,2)")
      assert(contains(singletonSet3, 3), "Singleton (s3,3)")
    }
  }

  test("singletonSet Sx does not contain y")
  {
    new TestSets
    {
      assert(!contains(singletonSet1,2), "Singleton (s1,2)")
      assert(!contains(singletonSet2,1), "Singleton (s2,1)")
    }
  }

  test("union contains all elements")
  {
    new TestSets
    {
      val unionSetNegativesS2 = union(negativeIntegerSet, singletonSet2)
      assert(contains(unionSetNegativesS2, -1), "union(negativeIntegerSet, singletonSet2): Union contains -1")
      assert(contains(unionSetNegativesS2, 2), "union(negativeIntegerSet, singletonSet2): Union contains 2")
      assert(!contains(unionSetNegativesS2, 3), "union(negativeIntegerSet, singletonSet2): Union should not contain 3")

      val unionSetNegativesS1 = union(singletonSet1,negativeIntegerSet)
      assert(contains(unionSetNegativesS1, 1), "union(singletonSet1,negativeIntegerSet): Union contains 1")
      assert(contains(unionSetNegativesS1, -11), "union(singletonSet1,negativeIntegerSet): Union contains -11")

      val unionSetNegativesPositives = union(negativeIntegerSet,positiveIntegerSet)
      assert(contains(unionSetNegativesPositives, -100), "union(negativeIntegerSet,positiveIntegerSet): Union contains -100")
      assert(contains(unionSetNegativesPositives, 100), "union(negativeIntegerSet,positiveIntegerSet): Union contains 100")
    }
  }

  test("intersect contains only elements in both sets")
  {
    new TestSets{
      val intersectionSetPositiveS1 = intersect(singletonSet1,positiveIntegerSet)
      assert(contains(intersectionSetPositiveS1, 1), "intersect(singletonSet1,positiveIntegerSet): Intersection contains 1")
      assert(!contains(intersectionSetPositiveS1, 2), "intersect(singletonSet1,positiveIntegerSet): Intersection should not contain 2")

      val intersectionSetNegativeS1 = intersect(singletonSet1,negativeIntegerSet)
      assert(!contains(intersectionSetNegativeS1, -99), "intersect(singletonSet1,negativeIntegerSet): Intersection should not contain -99")
      assert(!contains(intersectionSetNegativeS1, 2), "intersect(singletonSet1,negativeIntegerSet): Intersection should not contain 2")
    }
  }

  test("diff contains all the elements of the set s that are not in the set t")
  {
    new TestSets
    {
      val diffSetPositiveS1 = diff(singletonSet1, positiveIntegerSet)
      assert(!contains(diffSetPositiveS1,1), "diff(singletonSet1, positiveIntegerSet): diff contains -1 ")
      assert(!contains(diffSetPositiveS1,1), "diff(singletonSet1, positiveIntegerSet): diff should not contain 1 ")

      val diffSetPositiveS2 = diff(positiveIntegerSet, singletonSet2)
      assert(contains(diffSetPositiveS2, 3), "diff(positiveIntegerSet, singletonSet2): diff contains 1 ")
      assert(!contains(diffSetPositiveS2, 2), "diff(positiveIntegerSet, singletonSet2): diff should not contain 2 ")
    }
  }

  test("filter returns the subset of `s` for which `p` holds")
  {
    new TestSets
    {
      val filterSet = filter(singletonSet3, singletonSet3)
      assert(contains(filterSet,3), "filter(s3,s3): filter contains 3")
      assert(!contains(filterSet,2), "filter(s3,s3): filter should not contain 2")

      val filterSetAllIntegers = filter(negativeIntegerSet,positiveIntegerSet)
      assert(!contains(filterSetAllIntegers,3), "filter(negativeIntegerSet,positiveIntegerSet): filter should not contain 3")
      assert(!contains(filterSetAllIntegers,-12), "filter(negativeIntegerSet,positiveIntegerSet): filter should not contain -12")
    }
  }

  test("for all returns whether all bounded integers within `s` satisfy `p`.")
  {
    new TestSets
    {
      val forAllSetAllIntegers = forall(negativeIntegerSet,positiveIntegerSet)
      assert(!forAllSetAllIntegers, "forAll(negativeIntegerSet,positiveIntegerSet): forall returns false for all negative integers satisfying positive integers")

      val forAllSetS3Positives = forall(singletonSet3,positiveIntegerSet)
      assert(forAllSetS3Positives, "forAll(singletonSet3,positiveIntegerSet): forall returns true for all s3 satisfying positive integers")

      val forAllSetPositivesS3 = forall(positiveIntegerSet,singletonSet3)
      assert(!forAllSetPositivesS3, "forAll(singletonSet3,positiveIntegerSet): forall returns false for all positive integers satisfying s3")

    }
  }

  test("exists returns whether there exists a bounded integer within `s`\n   * that satisfies `p` ")
  {
    new TestSets
    {
      val existsSetNegativeS1 = exists(negativeIntegerSet, singletonSet1)
      assert(!existsSetNegativeS1, "exists(negativeIntegerSet, singletonSet1) : exists returns false such that 1 does not exist in all negative integers)")

      val existsSetPositiveS1 = exists(positiveIntegerSet, singletonSet1)
      assert(existsSetPositiveS1, "exists(positiveIntegerSet, singletonSet1) : exists returns true such that 1 exists in all positive integers)")

      val existsSetPositiveSetXMod2S3 = exists(positiveSetXMod2, singletonSet3)
      assert(existsSetPositiveSetXMod2S3, "exists(positiveXPlus2Set, singletonSet1) : exists returns true such that 3 exists in all positive integers x mod 2)")

      val existsSetPositiveSetXMod2S4 = exists(positiveSetXMod2, singletonSet4)
      assert(!existsSetPositiveSetXMod2S4, "exists(positiveXPlus2Set, singletonSet1) : exists returns true such that 4 exists in all positive integers x mod 2)")
    }


  }

}
