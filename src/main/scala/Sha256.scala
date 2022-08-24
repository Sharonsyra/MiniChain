import Base.{Bytes, Hash}

import java.security.MessageDigest

// Hashes are produced by a cryptographic function and in "mini-chain" we
// use SHA-256, which always generates a 32-byte (256-bit) value.
object Sha256 {
  val NumberOfBytes: Int = 32
  val TheDigest: MessageDigest = MessageDigest.getInstance("SHA-256")

  // We pre-compute the hash of an empty array of 32 bytes.
  // We call this the "Zero_Hash".
  TheDigest.update(Bytes(32))
  val Zero_Hash: Hash = Hash(TheDigest.digest())

  // We use this to hash a composite structure whose constituents can be given
  // as byte arrays. We just feed everything to SHA-256.
  def apply(bytess: Bytes*): Hash = {
    for (bytes <- bytess) {
      TheDigest.update(bytes)
    }

    val hash = TheDigest.digest()
    assert(hash.length == NumberOfBytes)

    Hash(hash)
  }
}
