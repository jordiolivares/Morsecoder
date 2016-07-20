case class Config(sep: String = "", encode: Boolean = false, decode: Boolean = false,
                  input: String = "")

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

    def decode(s: String, sep: String = """\s{2,}""") = {
        s.split(sep).map( (word) =>
            word.trim.split(" ") // For each morse code char
            .map(decoderMap(_)) // Decode
            .mkString("") // and join
        ).mkString(" ") // and join the words
    }

    def encode(s: String, sep: String = "  ") = s.trim.toUpperCase.split("""\s+""").map( (word) =>
        word.map(coderMap(_)).mkString(" "))
        .mkString(" " + sep + " ")

    val parser = new scopt.OptionParser[Config]("Morsecoder") {
        head("Morsecoder", "0.1")

        opt[Unit]('e', "encode").
            action( (_, config) =>
                if (config.decode == false)
                    config.copy(encode = true)
                else
                    config
            ).text("Encodes the given text in Morse code using the separator to separate the end result")

        opt[Unit]('d', "decode").
            action( (_, config) =>
                if (config.encode == false)
                    config.copy(decode = true)
                else
                    config
            ).text("Decodes the given Morse code using the separator to differentiate between words")

        opt[String]('s', "separator").
            action( (x, config) => config.copy(sep = x) ).
            valueName("SEP").
            text("uses the given separator SEP to differentiate between words in the Morse code")

        opt[String]('i', "input").
            required().
            action( (x, config) => config.copy(input = x)).
            valueName("TEXT").
            text("The text to be encoded/decoded")
    }

    def main(args: Array[String]): Unit = {
        def localEncode(s: String, sep: String) = {
            sep match {
                case "" => println(encode(s))
                case _ => println(encode(s, sep))
            }
        }

        def localDecode(s: String, sep: String) = {
            sep match {
                case "" => println(decode(s))
                case _ => println(decode(s, sep))
            }
        }

        parser.parse(args, Config()) match {
            case Some(config) => {
                val text = config.input
                val sep = config.sep
                if ((config.decode | config.encode) == false) {
                    println("Error: Please choose whether to decode or encode")
                    println(parser.usage)
                } else
                    config.decode match {
                        case true => localDecode(text, sep)
                        case false => localEncode(text, sep)
                    }
            }
            case None =>
        }
    }

}
