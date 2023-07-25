object CaeserCipher {

  def Encrypt(plaintext: String, shift: Int): String = {

    val encryptedText = plaintext.map { char =>
      
      if (char.isLetter) {
        val shiftAmount = if (char.isUpper) 'A' else 'a'
        val encryptedChar = (((char - shiftAmount + shift) % 26 + 26) % 26 + shiftAmount).toChar
        encryptedChar
      }
      else {
        char
      }
    }
    encryptedText
  }

  def Decrypt(ciphertext: String, shift: Int): String = {
    Encrypt(ciphertext, -shift)
  }

  def Cipher(text: String, shift: Int, func: (String, Int) => String): String = {

    func(text, shift)
    
  }

  def main(args: Array[String]): Unit = {

    print("Enter the text to encrypt : ")
    val plaintext = scala.io.StdIn.readLine()
    print("Enter the shift value : ")
    val shift = scala.io.StdIn.readInt()

    val encrypted = Cipher(plaintext, shift, Encrypt)
    println(s"Encrypted text: $encrypted")

    val decrypted = Cipher(encrypted, shift, Decrypt)
    println(s"Decrypted text: $decrypted")
  }
}
