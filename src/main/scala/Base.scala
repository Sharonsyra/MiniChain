object Base {
  // When you see Unknown, replace it with appropriate type on a per
  // case basis. So if you see:
  //
  //   def append(block: Block): Unknown
  //
  // and later in the code:
  //
  //   def findByIndex(index: Int): Unknown
  //
  // and we ask you to implement the methods, you do not necessarily
  // have to replace Unknown with the same type in both cases.
  type Unknown = Nothing

  type Nonce = Long

  type Bytes = Array[Byte]
  val Bytes: Int => Array[Byte] = new Array[Byte](_: Int)

  type Number = BigInt
  val Number: BigInt.type = BigInt

  def toHexString(bytes: Array[Byte]): String =
    "0x" + bytes.map(b => String.format("%02X", Byte.box(b))).mkString("")

  // The idea behind any cryptographic hash representation in "mini-chain"
  // is to treat it as an immutable array of bytes that can be also viewed
  // as a number or a hex string. You will see that the number representation
  // is used in the mining process. The hex representation is for logging
  // purposes.
  case class Hash(bytes: Bytes) {
    def toNumber: Number = Number(1, bytes)

    def toHexString: String = Base.toHexString(bytes)
  }

  // In "mini-chain", a transaction is just a message.
  case class Transaction(data: String)
}
