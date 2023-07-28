import scala.io.StdIn      //importing files for user input

object CaeserCipher{

    def Encryption(plaintext : String , shift : Int) : String = {

        val encrypted = plaintext.map { char =>  //map method will be implemented on plaintext separately traversing every character of the text.(high - order function)

            if(char.isLetter){ //method to check whether it is an alphebetical character or any other special character.

                val unicodeValue = if(char.isUpper) 'A' else 'a'// if true, assigning unicode values for the compiler to understand

                val unicodeValueofChar = (((char - unicodeValue + shift ) % 26 + 26 ) % 26 + unicodeValue).toChar //caeser cipher algrithm
                unicodeValueofChar
            }
            else{
                char
            }

        }

        encrypted
    }

    def Decryption( plaintext : String , shift : Int) : String = {

        val encryptedText = Encryption(plaintext,shift)
        Encryption(encryptedText, -shift) // vise versa of encryption
    }

    def chooseCipherMethod(text : String, shift : Int , method : (String , Int) => String) : String = {

        method(text,shift) //defining a method to choose a method
    }




    def main(args: Array[String]): Unit = {
        
        println("Enter the plaintext : ")
        val plaintext  =  StdIn.readLine()

        println("Enter the shift value : ")
        val shift = StdIn.readInt()

        println(s"Encrypted text : ${chooseCipherMethod(plaintext,shift,Encryption)}")

        println(s"Decrypted text : ${chooseCipherMethod(plaintext,shift,Decryption)}")
    }
}