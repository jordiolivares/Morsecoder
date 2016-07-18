object Morsecoder {

    val coderMap = Map(
        // Letters
        'A' -> ".-", 'B' -> "-...", 'C' -> "-.-.", 'D' -> "-..", 'E' -> ".",
        'F' -> "..-.", 'G' -> "--.", 'H' -> "....", 'I' -> "..", 'J' -> ".---",
        'K' -> "-.-", 'L' -> ".-..", 'M' -> "--", 'N' -> "-.", 'O' -> "---",
        'P' -> ".--.", 'Q' -> "--.-", 'R' -> ".-.", 'S' -> "...", 'T' -> "-",
        'U' -> "..-", 'V' -> "...-", 'W' -> ".--", 'X' -> "-..-", 'Y' -> "-.--",
        'Z' -> "--..",
        // Numbers
        '1' -> ".----", '2' -> "..---", '3' -> "...--", '4' -> "....-",
        '5' -> ".....", '6' -> "-....", '7' -> "--...", '8' -> "---..",
        '9' -> "----.", '0' -> "-----",
        // Punctuation and blank spaces
        '@' -> ".--.-.", '.' -> ".-.-.-", ',' -> "--..--", '?' -> "..--..",
        '\'' -> ".----.", '!' -> "-.-.--", '/' -> "-..-.", '(' -> "-.--.",
        ')' -> "-.--.-", '&' -> ".-...", ':' -> "---...", ';' -> "-...-",
        '+' -> ".-.-.", '-' -> "-....-", '_' -> "..--.-", '"' -> ".-..-."
        )

    val decoderMap = coderMap.map(_.swap)

    def decode(s: String) = {
        s.split("""\s{2,}""").map( (word) =>
            word.split(" ") // For each morse code char
            .map(decoderMap(_)) // Decode
            .mkString("") // and join
        ).mkString(" ") // and join the words
    }

    def encode(s: String) = s.toUpperCase.split("""\s+""").map( (word) =>
        word.map(coderMap(_)).mkString(" "))
        .mkString("  ")

    val helpMsg = """morsecoder {-e,-d} <arg>
                    |    -e    encodes a given string <arg>
                    |    -d    decodes a given morse code <arg>""".stripMargin('|')
    def main(args: Array[String]): Unit = {
        if (args.length < 2) 
            println(helpMsg)
        else args(0) match {
            case "-d" => println(decode(args(1)))
            case "-e" => println(encode(args(1)))
            case _    => println(helpMsg)
        }
    }

}
