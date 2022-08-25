package minichain

import minichain.Base.Hash
import minichain.Miner.Genesis

import scala.collection.mutable.ArrayBuffer

// A minichain.Blockchain is a sequence of blocks, each one having an index.
// The index of a block is the index of its parent plus one.
// A minichain.Blockchain always has a genesis block at index 0, which is the lowest index.
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
class FastBlockchain(chain: ArrayBuffer[Block] = ArrayBuffer[Block](Genesis)) extends Blockchain {
  private def error(msg: String) = throw new RuntimeException(msg)

  def append(block: Block): ArrayBuffer[Block] = {
    if (block.index != chain.last.index + 1) {
      error("minichain.Block's index is not properly aligned")
    } else if (block.parentHash.toNumber != chain.last.cryptoHash.toNumber) {
      error("minichain.Block's hash is not properly aligned")
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
