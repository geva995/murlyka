module IdKeywords(
  IdKeyw(..),
  is_id_char,
  idKeyword
) where
import qualified Data.Map as Maps
import qualified Data.Set as Sets

data IdKeyw=Project | Compiler | Compiler_flags | Linker | Linker_flags | Source_dir |
  Source_exts | Build_dir | Include_dirs | Makefile_name | Ident String
  deriving(Eq,Ord,Show)

id_chars::Sets.Set Char
id_chars=Sets.fromList$['A'..'Z']++['a'..'z']++['0'..'9']++['-','+','_','.']

is_id_char::Char->Bool
is_id_char c = c `Sets.member` id_chars

keywords::Maps.Map String IdKeyw
keywords=Maps.fromList[
  ("project",Project),
  ("compiler",Compiler),
  ("compiler_flags",Compiler_flags),
  ("linker",Linker),
  ("linker_flags",Linker_flags),
  ("source_dir",Source_dir),
  ("source_exts",Source_exts),
  ("build_dir",Build_dir),
  ("include_dirs",Include_dirs),
  ("makefile_name",Makefile_name)]

idKeyword::String->(String,IdKeyw)
idKeyword y=
  (hvst, maybe (Ident ik) id $ Maps.lookup ik keywords)
  where (ik,hvst)=span is_id_char y 