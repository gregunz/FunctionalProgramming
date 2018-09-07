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
  val olivier: List[Char] = string2Chars("BONJOURJESEEEEDJLFAJDLKSFJDLASFJLADAAAAAAAAEEEEEEEEEEEEEIIIIIIIIIIEEEEEEEEEUISOLIVIERDECARGLASSVOUSVOYEZCETIMPACT")


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



  test("print decodedSecret") {
    def loop(cs: List[Any]): Unit = cs match {
      case Nil => println()
      case _ => print(cs.head); loop(cs.tail)
    }
    val t = createCodeTree(olivier)
    //loop(decodedSecret)
    //loop(convert(frenchCode))
    //loop(times(string2Chars("arrbbadakkarbba")))
    //println(convert(frenchCode).head._2.head)
    //loop(combine(makeOrderedLeafList(times(olivier))))

    //loop(convert(createCodeTree(olivier)))
    //loop(quickEncode(t)(olivier))
  }

  test("decoded re-encoded"){
    assert(encode(frenchCode)(decodedSecret) === secret)
  }

  test("decoded quickEncoded"){
    assert(quickEncode(frenchCode)(decodedSecret) === secret)
  }

  test("encode == quickEncode") {
    val t = createCodeTree(olivier)
    assert ( quickEncode(t)(olivier) == encode(t)(olivier) )
  }


  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("olivier bits after encoding") {
    assert( encode(createCodeTree(olivier))(olivier).length === "1110000 11110 1110111 0001 11110 00101 111111 0001 10 0101 10 10 10 10 0000 0001 0011 01001 011 0001 0000 0011 1110100 0101 01001 0001 0000 0011 011 0101 01001 0001 0011 011 0000 011 011 011 011 011 011 011 011 10 10 10 10 10 10 10 10 10 10 10 10 10 110 110 110 110 110 110 110 110 110 110 10 10 10 10 10 10 10 10 10 00101 110 0101 11110 0011 110 01000 110 10 111111 0000 10 00100 011 111111 1110001 0011 011 0101 0101 01000 11110 00101 0101 01000 11110 1110110 10 1111100 00100 10 111001 110 1111101 1110101 011 00100 111001 "
      .replaceAll(" ","").length())
  }

  test("frenchCode == create") {
  }

}
