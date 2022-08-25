package minichain

import minichain.Base.{Hash, Transaction}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class BlockSpec extends AnyWordSpec with Matchers {
  val blk: Block =
    Block(
      index = 0,
      parentHash = Sha256.Zero_Hash,
      transactions = Seq(Transaction("Test Transaction")),
      miningTargetNumber = -1,
      nonce = -1
    )
  "The Block" should {
    "return a hash object successfully" in {
      blk.cryptoHash shouldBe a[Hash]
    }

    "verify proper mining" in {
      assertThrows[java.lang.AssertionError]{
        blk.verifyThisHasBeenMinedProperly()
      }
    }
  }
}
