package minichain

import minichain.Base.Transaction
import minichain.Miner.StdMiningTargetNumber
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class MinerSpec extends AnyWordSpec with Matchers {
  "The miner" should {
    "mine the next block" in {
      val Genesis = Miner.mineNextBlock(
        index = 0, // The very first block
        parentHash = Sha256.Zero_Hash, // Let's assume this is by definition for the Genesis block.
        transactions = Seq(Transaction("Hello Blockchain, this is Genesis :)")),
        StdMiningTargetNumber,
      )

      val response: Block = Miner.mineNextBlock(
        index = 1,
        parentHash = Genesis.parentHash,
        transactions = Seq(Transaction("Mining Test Transaction")),
        miningTargetNumber = StdMiningTargetNumber
      )

      response.index shouldBe 1
    }
  }
}
