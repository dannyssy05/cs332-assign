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
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }
  // ----- 추가 테스트 시작 -----

  test("singleton: true for single tree, false otherwise") {
    assert(singleton(List(Leaf('t', 2))))
    assert(!singleton(List(Leaf('t', 2), Leaf('x', 3))))
    assert(!singleton(Nil))
  }

  test("combine: empty and singleton lists unchanged") {
    assert(combine(Nil) === Nil)
    val one = List(Leaf('x', 1))
    assert(combine(one) === one)
  }

  test("until reduces to a single tree") {
    val trees = makeOrderedLeafList(List(('a',2), ('b',3), ('c',1)))
    val result = until(singleton, combine)(trees)
    assert(result.size === 1)
    assert(weight(result.head) === 6)
    assert(chars(result.head).toSet === Set('a','b','c'))
  }

  test("createCodeTree produces correct total weight") {
    val text = "aabbbcccc".toList
    val tree = createCodeTree(text)
    assert(weight(tree) === text.size)
    assert(chars(tree).toSet === text.toSet)
  }



  test("encode and decode with createCodeTree should be identity on a phrase") {
    val text = "the quick brown fox jumps over the lazy dog".toList
    val tree = createCodeTree(text)
    val enc  = encode(tree)(text)
    val dec  = decode(tree, enc)
    assert(dec === text)
  }

  test("quickEncode equals encode") {
    val text = "vfdoijioFIONFJIOfDMIOmklvfdsmiojfvdklmzvvmilAIJVDSMILrsdjiodsvmlkVSDSVNIOmklfvzs".toList
    val tree = createCodeTree(text)
    val e1   = encode(tree)(text)
    val e2   = quickEncode(tree)(text)
    assert(e1 === e2)
  }

  test("convert builds a consistent CodeTable (lookup matches encode path)") {
    val text = "abbacaba".toList
    val tree = createCodeTree(text)
    val table = convert(tree)
    // table lookup helper
    val lookup = codeBits(table) _
    // check that concatenating table lookups equals encode result
    val viaTable = text.flatMap(lookup)
    assert(viaTable === encode(tree)(text))
  }

  test("codeBits: simple table lookup") {
    val table = List(('a', List(0)), ('b', List(1,0)))
    assert(codeBits(table)('a') === List(0))
    assert(codeBits(table)('b') === List(1,0))
  }

  // ----- French tree (frenchCode/secret) 관련 테스트 -----

  test("frenchCode: secret decodes to the expected message") {
    val msg = decode(frenchCode, secret)
    assert(msg.mkString === "huffmanestcool")   // "Huffman est cool"
  }

  test("frenchCode: decodedSecret value matches decode(frenchCode, secret)") {
    assert(decodedSecret === decode(frenchCode, secret))
  }
//

  test("times counts frequencies (order independent)") {
    val freq = times(List('a','a','b','c','c','c'))
    assert(freq.toSet === Set(('a',2), ('b',1), ('c',3)))
  }

  test("mergeCodeTables concatenates two code tables") {
    val t1: CodeTable = List(('a', List(0,0)), ('b', List(0,1)))
    val t2: CodeTable = List(('c', List(1)))
    val merged = mergeCodeTables(t1, t2)
    assert(merged === List(('a', List(0,0)), ('b', List(0,1)), ('c', List(1))))
  }

  test("decode after quickEncode should be identity") {
    val text = "thequickbrownfox".toList
    val tree = createCodeTree(text)
    val encoded = quickEncode(tree)(text)
    assert(decode(tree, encoded) === text)
  }

  test("convert builds expected codes on a small deterministic tree") {
    val small = Fork(
      Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5),
      Leaf('c',4),
      List('a','b','c'),
      9
    )
    val table = convert(small)
    val lookup = codeBits(table) _
    assert(lookup('a') === List(0,0))
    assert(lookup('b') === List(0,1))
    assert(lookup('c') === List(1))
  }







}
