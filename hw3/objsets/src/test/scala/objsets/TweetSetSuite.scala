package objsets

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TweetSetSuite extends FunSuite {
  trait TestSets {
    val set1 = new Empty
    val set2 = set1.incl(new Tweet("a", "a body", 20))
    val set3 = set2.incl(new Tweet("b", "b body", 20))
    val c = new Tweet("c", "c body", 7)
    val d = new Tweet("d", "d body", 9)
    val set4c = set3.incl(c)
    val set4d = set3.incl(d)
    val set5 = set4c.incl(d)
  }

  def asSet(tweets: TweetSet): Set[Tweet] = {
    var res = Set[Tweet]()
    tweets.foreach(res += _)
    res
  }

  def size(set: TweetSet): Int = asSet(set).size

  test("filter: on empty set") {
    new TestSets {
      val filterEmptySet = set1.filter(tw => tw.user == "a")
      assert(size(filterEmptySet) === 0)
    }
  }

  test("filter: a on set5") {
    new TestSets {
      val filterAOnSetFive = set5.filter(tw => tw.user == "a")
      assert(size(filterAOnSetFive) === 1)
    }
  }

  test("filter: 20 on set5") {
    new TestSets {
      val filter20OnSet5 = set5.filter(tw => tw.retweets == 20)
      assert(size(filter20OnSet5) === 2)
    }
  }

  test("union: set4c and set4d") {
    new TestSets {
      val unionSet4cAndSet4d = set4c.union(set4d)
      assert(size(unionSet4cAndSet4d) === 4)
    }
  }

  test("union: with empty set (1)") {
    new TestSets {
      val unionWithEmptySet1 = set5.union(set1)
      assert(size(unionWithEmptySet1) === 4)
    }
  }

  test("union: with empty set (2)") {
    new TestSets {
      val unionWithEmptySet2 = set1.union(set5)
      assert(size(unionWithEmptySet2) === 4)
    }
  }

  test("mostRetweet with set4c") {
    new TestSets {
      val set4cMostRetweeted = set4c.mostRetweeted
      println(set4cMostRetweeted)
      assert(set4cMostRetweeted.retweets == 20)
    }
  }

  test("descending: empty set1") {
    new TestSets {
      val trends = set1.descendingByRetweet
      assert(trends.isEmpty)
    }
  }

  test("descending: set5") {
    new TestSets {
      val trends = set5.descendingByRetweet
      assert(!trends.isEmpty)
      assert(trends.head.user == "a" || trends.head.user == "b")
    }
  }
}
