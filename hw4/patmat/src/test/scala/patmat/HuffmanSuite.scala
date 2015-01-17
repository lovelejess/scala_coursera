package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
  trait TestTrees {
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
    val l1 = List('e','e','t','x','x','x','x')
    val leafList =  List(Leaf('e', 2), Leaf('t', 1), Leaf('x', 4))
  }

  test("weight of a t1 tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("weight of the t2 tree") {
    new TestTrees{
      assert(weight(t2) == 9)
    }
  }

  test("chars of a t2 tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }

  test("chars of a t1 tree") {
    new TestTrees {
      assert(chars(t1) === List('a','b'))
    }
  }

  test("findMatch of L1"){
    new TestTrees{
      assert(findMatch('e',List('e','e','e'), 0)=== ('e',3))
      assert(findMatch('e',l1,0) === ('e',2))
      assert(findMatch('x',l1,0) === ('x',4))
      assert(findMatch('t',l1,0) === ('t',1))
    }
  }

  test("times of L1"){
    new TestTrees{
      assert(times(l1) === List(('e',2),('t',1), ('x',4)))
    }
  }
  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("singletonList List(t1,t2) == false "){
    new TestTrees {
      val codeTree = List(t1,t2)
      assert(singleton(codeTree) === false)
    }
  }

  test("singletonList List(t1) == true "){
    new TestTrees {
      val codeTree = List(t1)
      assert(singleton(codeTree) === true)
    }
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 2), Leaf('t', 1), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',2),Leaf('t',1),List('e', 't'),3), Leaf('x',4)))
  }

  test("until a list is a singleton tree"){
    val leaflist = List(Leaf('e', 2), Leaf('t', 1), Leaf('x', 4))
    assert(until(singleton, combine)(leaflist) ===
      List(Fork(Fork(Leaf('e',2),Leaf('t',1),List('e', 't'),3),Leaf('x',4),List('e', 't', 'x'),7)))
  }

  test("createCodeTree ") {
    val charsList = List('e', 'x', 't', 'e', 'x', 'x', 'x')
    assert(createCodeTree(charsList) === (Fork(Fork(Leaf('e', 2), Leaf('t', 1), List('e', 't'), 3), Leaf('x', 4), List('e', 't', 'x'), 7)))
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }
}
