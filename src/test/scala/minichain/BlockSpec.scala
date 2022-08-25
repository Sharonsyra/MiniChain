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
      miningTargetNumber = Miner.StdMiningTargetNumber,
      nonce = Miner.targetByLeadingZeros(1).toLong
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
