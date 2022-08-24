import Base.{Bytes, Hash, Transaction}

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

object Miner {
  // NOTE: A Hash is also a Number, we use the two interchangeably.
  //
  // Mining is about computing hashes until we get something that is less
  // than a given target number.
  // This target serves, in a way, as the maximum possible number that a
  // proof of work computation should produce.
  final val StdMiningTargetNumber = targetByLeadingZeros(1)

  // Whoa! We actually mine the Genesis block.
  // Normally, this is done by the system during bootstrapping
  // and every other block is mined by a miner.
  final val Genesis = Miner.mineNextBlock(
    index = 0, // The very first block
    parentHash = Sha256.Zero_Hash, // Let's assume this is by definition for the Genesis block.
    transactions = Seq(Transaction("Hello Blockchain, this is Genesis :)")),
    StdMiningTargetNumber,
  )

  // We basically create a target number with the requirement of having
  // some leading zeros. More leading zeros means smaller target number.
  //
  // NOTE: To actually solve the current coding challenge, would you choose a
  // small or a big number of leading zeros?
  def targetByLeadingZeros(zeros: Int): BigInt = {
    require(zeros < Sha256.NumberOfBytes)

    val bytes: Bytes =
      Array.tabulate[Byte](32) { n =>
        if (n < zeros) {
          0
        }
        else {
          0xff.toByte
        }
      }
    BigInt(1, bytes)
  }

  // And now let's implement the actual "proof-of-work"-style computation.
  // Compare the parameters of this method with the fields of a Block and
  // you'll see that the only thing missing here is the nonce. Here is why.
  //
  // Initially we have all the fixed elements a block:
  //
  //  - index,
  //  - parentHash,
  //  - transactions,
  //  - miningTargetNumber
  //
  // and by varying the nonce we try to have a block hash that is below the
  // given miningTargetNumber.
  //
  // NOTE Remember that the block hash can be transformed to an actual number,
  //      so we can talk about hash and number interchangeably.
  def mineNextBlock(
    index: Int,
    parentHash: Hash,
    transactions: Seq[Transaction],
    miningTargetNumber: BigInt,
  ): Block = {
    // Solve this informal inequality for nonce:
    //
    //   Hash(block; nonce).toNumber < miningTargetNumber
    //
    // where Hash(block; nonce) is a function of nonce only, all the other block
    // field values are just the given method arguments.

    @tailrec
    def findNonce(currentNonce: Long): Long = {
      val block = Block(index, parentHash, transactions, miningTargetNumber, currentNonce)
      if (block.verifyThisHasBeenMinedProperly()) currentNonce
      else findNonce(currentNonce + 1)
    }

    val nonce = findNonce(0L)
    Block(index, parentHash, transactions, miningTargetNumber, nonce)
  }

  // A Blockchain is a sequence of blocks, each one having an index.
  // The index of a block is the index of its parent plus one.
  // A Blockchain always has a genesis block at index 0, which is the lowest index.
  sealed trait Blockchain {

    // Add a block to the chain.
    // The return type is up to you, as explained in the definition of Unknown.
    def append(block: Block): ArrayBuffer[Block]

    // Find a block by index.
    def findByIndex(index: Int): Option[Block]

    // Find a block by hash.
    def findByHash(hash: Hash): Option[Block]

    // Find a common ancestor between this blockchain and that blockchain.
    def common_ancestor(that: Blockchain): Option[Block]
  }

  // Implement an in-memory blockchain that internally has an indexing data structure.
  // The purpose of this internal data structure is to avoid traversing the linked list
  // of blocks when answering queries like findByIndex.
  class FastBlockchain(chain: ArrayBuffer[Block] = ArrayBuffer(Genesis)[Block]) extends Blockchain {
    private def error(msg: String) = throw new RuntimeException(msg)

    def append(block: Block): ArrayBuffer[Block] = {
      if (block.index != chain.last.index + 1) {
        error("Block's index is not properly aligned")
      } else if (block.parentHash.toNumber != chain.last.cryptoHash.toNumber) {
        error("Block's hash is not properly aligned")
      }
      else {
        chain.append(block)
      }
    }

    def findByIndex(index: Int): Option[Block] = {
      if(chain.size - 1 < index) {
        error("index out of range")
      } else {
        Some(chain(index))
      }
    }

    def findByHash(hash: Hash): Option[Block] = {
      chain.find(block => block.cryptoHash == hash)
    }

    // Checks if the two chains have the same common ancestor. Used to test for forks
    def common_ancestor(that: Blockchain): Option[Block] = {
      if (that.findByIndex(0).contains(chain(0))) {
        Some(chain(0))
      } else None
    }
  }
}
