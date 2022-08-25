package minichain

import minichain.Base.{Hash, Transaction}
import org.scalatest.OptionValues
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.collection.mutable.ArrayBuffer

class FastBlockChainSpec extends AnyWordSpec with Matchers with OptionValues {
  val chain: FastBlockchain = new FastBlockchain(
    ArrayBuffer[Block](
      Block(
        index = 0,
        parentHash = Sha256.Zero_Hash,
        transactions = Seq(Transaction("Test Transaction")),
        miningTargetNumber = -1,
        nonce = -1
      )
    )
  )

  "The FastBlockChain" should {
    "have a Genesis record when instantiated" in {
      val genesisBlock: Option[Block] = chain.findByIndex(0)
      genesisBlock.value.parentHash.toNumber shouldBe Sha256.Zero_Hash.toNumber
    }

    "append should add a new valid block to the chain" in {
      val genesisBlock: Option[Block] = chain.findByIndex(0)
      val secondBlock: Block = Block(
        index = 1,
        parentHash = genesisBlock.value.cryptoHash,
        transactions = Seq(Transaction("Test Transaction 2")),
        miningTargetNumber = -1,
        nonce = -1
      )
      chain.append(secondBlock)
      secondBlock.parentHash.toNumber shouldBe genesisBlock.value.cryptoHash.toNumber
    }

    "append should throw an exception when adding a block with an invalid index" in {
      val genesisBlock: Option[Block] = chain.findByIndex(0)
      assertThrows[RuntimeException]{
        val secondBlock = Block(
          index = 3,
          parentHash = genesisBlock.value.cryptoHash,
          transactions = Seq(Transaction("Test Transaction fail")),
          miningTargetNumber = -1,
          nonce = -1
        )
        chain.append(secondBlock)
      }
    }

    "find by index should return block by valid index" in {
      val response: Option[Block] = chain.findByIndex(0)
      response shouldBe defined
      response.value.index == 0
      response should !== (None)
    }

    "find by index should throw an exception when invalid index range is added" in {
      assertThrows[RuntimeException]{
        chain.findByIndex(9)
      }
    }

    "find by hash should return block if it exists in chain" in {
      val genesisBlock: Option[Block] = chain.findByIndex(0)
      val response = chain.findByHash(genesisBlock.value.cryptoHash)
      response shouldBe defined
      response.value.index shouldBe 0
      response should !== (None)
    }

    "find by hash should return None when given a wrong hash" in {
      val response: Option[Block] =
        chain.findByHash(Hash("a82af4e96b5b8d825a6910b908eccdc4401af1e663db5edf2d7dfb712db96529".getBytes))
      response should === (None)
    }
  }
}
