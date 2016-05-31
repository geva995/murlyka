module UnknownLexem(
  unknownLexem
) where 
import qualified Data.Set as Sets

unknownLexem::String->String
unknownLexem = dropWhile is_unknown_char 

is_unknown_char c = not $ c `Sets.member` known_chars

known_chars :: Sets.Set Char 
known_chars = 
  Sets.fromList $ ['A'..'Z']++['a'..'z']++['0'..'9']++['+','-','_','.','\"','(',')']