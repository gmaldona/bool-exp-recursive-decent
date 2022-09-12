import scala.util.matching.Regex

case class S();
case class Assign();
case class Query();
case class Exp();

object yylex {

    def peek(): Unit = {

    }

    def advance(): Unit = {

    }
}

object Lexer {

    def main(args: Array[String]) = {
        val test_str = "a?"

        if (test_str.contains("?"))
        {
            val id_pattern = "[a-z]".r
            val id = ""; 
            
            id_pattern.findFirstMatchIn(test_str) match {
                case Some(x) => id = x;
                case None => ;
            }

            Query(); 
        }
        else 
        {

        }
    }

}