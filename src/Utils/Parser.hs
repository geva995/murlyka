module Parser(
  parser,
  Info(..),
  Project_Title(..)
) where
import qualified Data.List as Lists
import qualified Data.Map as Maps
import IdKeywords
import Delimiters
import Data.Array
import qualified Scaner as SC


data LexemClass= ClassA | ClassB | ClassC | ClassK | ClassM | ClassL | ClassN 
  deriving (Eq,Ord,Show)

data Info = Info { project::Project_Title ,
                   compiler:: String ,
                   compiler_flags::String ,
                   linker::String ,
                   linker_flags::String ,
                   source_dir::String ,
                   source_exts::String ,
                   build_dir::String ,
                   include_dirs::String ,
                   makefile_name::String }
  deriving (Eq,Ord,Show)

data Project_Title=Project_Title{name::String,main_file::String}
  deriving(Eq, Ord, Show)

data ParserState = A | B | C | D | E | F | G | H | I
  deriving (Eq,Ord,Show,Enum,Ix)

type Message = String

emptyProject::Project_Title
emptyProject = Project_Title{name="", main_file=""}

emptyInfo::Info
emptyInfo= 
  Info {project=emptyProject,
        compiler="",
        compiler_flags="",
        linker="",
        linker_flags="",
        source_dir="",
        source_exts="",
        build_dir="",
        include_dirs="",
        makefile_name=""}

parser ::[SC.Lexem]->Either String Info
parser ls =
  if not . null $ messages' then
    Left messages'
  else 
    Right info
  where
    (_,_,info,messages)=Lists.foldl' nextParserAccum (A,head ls,emptyInfo,[]) ls
    messages'= unlines . filter (not . null) . reverse $ messages

type ParserAccum=(ParserState,SC.Lexem,Info,[Message])
{-- 
  Первый элемент кортежа --- текущее состояние парсера,
  второй элемент --- сохранённая на данный момент лексема,
  третий элемент --- текущие сведения,
  четвёртый элемент --- список сообщений об ошибке.
--}

nextParserAccum :: ParserAccum -> SC.Lexem -> ParserAccum
nextParserAccum (st,pl,i,msgs) l = (st',pl',i',msg':msgs)
  where 
    (st',msg')= jump st l
    pl'       = prot_lex pl l
    i'        = modification pl' st l i

jump::ParserState->SC.Lexem->(ParserState,String)
jump st l@(_, n)=
  case Maps.lookup (st,lc) usual_jumps of
    Nothing -> avral_jump (st,lc) n
    Just st'-> (st',"")
  where lc= classification l 

usual_jumps::Maps.Map (ParserState,LexemClass) ParserState
usual_jumps=Maps.fromList [((A,ClassA),B),
                           ((A,ClassB),C),
                           ((A,ClassC),D),
                           ((B,ClassK),E),
                           ((C,ClassK),F),
                           ((D,ClassK),G),
                           ((E,ClassM),H),
                           ((F,ClassM),I),
                           ((G,ClassN),I),
                           ((H,ClassN),I),
                           ((H,ClassL),A),
                           ((I,ClassL),A)]

avral_jump::(ParserState,LexemClass)->Int->(ParserState,String)
avral_jump (st,ClassA) n = (B,(diagnostic ! st) n)
avral_jump (st,ClassB) n = (C,(diagnostic ! st) n)
avral_jump (st,ClassC) n = (D,(diagnostic ! st) n)
avral_jump (st,ClassL) n = (A,(diagnostic ! st) n)
avral_jump (st,ClassN) n = (I,(diagnostic ! st) n)
avral_jump (st,ClassK) n =
  case st of
    A->(E,d)
    E->(E,d)
    F->(F,d)
    G->(G,d)
    H->(A,d)
    I->(A,d)
    _->(E,d)
  where d=(diagnostic ! st) n
avral_jump (st,ClassM) n =
  case st of 
    A->(H,d)
    B->(H,d)
    C->(I,d)
    D->(I,d)
    G->(I,d)
    H->(A,d)
    I->(A,d)
    _->(H,d)
  where d=(diagnostic ! st) n

diagnostic::Array ParserState (Int->String)
diagnostic = listArray (A,I) [
  expected_abc,expected_k,
  expected_k,expected_k,
  expected_m,expected_m,
  expected_n,expected_ln,
  expected_l ]

expected_abc::Int->String
expected_abc n = "В строке "++ show n++ " ожидается ключевое слово." 

expected_k::Int->String
expected_k n = "В строке "++ show n ++ " ожидается (."

expected_m::Int->String
expected_m n = "В строке " ++ show n ++ " ожидается идентификатор."

expected_l::Int->String
expected_l n = "В строке " ++ show n ++ " ожидается )."

expected_n::Int->String
expected_n n = "В строке " ++ show n ++ " ожидается строковый литерал."

expected_ln::Int->String
expected_ln n = "В строке " ++ show n ++ " ожидается ) или строковый литерал."

classification::SC.Lexem->LexemClass
classification (t,_)=
  case t of
    SC.IK ik -> ik_classification ik
    SC.D d   -> d_classification d
    SC.Str _ -> ClassN
    _        -> ClassM

ik_classification::IdKeyw->LexemClass
ik_classification (Ident _)=ClassM
ik_classification i
  |(i==Project)                               = ClassA
  |(i `elem` [Compiler,Linker,Makefile_name]) = ClassB
  |otherwise                                 = ClassC

d_classification::Delims->LexemClass
d_classification LP=ClassK
d_classification RP=ClassL

modification :: SC.Lexem -> ParserState -> SC.Lexem -> Info -> Info
modification (old_t,_) st l@(t,_) i
  |(st `elem` [A,B,E])   && (lc==ClassM) = setProjectName i t
  |(st `elem` [C,F])     && (lc==ClassM) = set_str_field old_t i . fromIdent . fromIK $ t
  |(st `elem` [A,B,E,H]) && (lc==ClassN) = setMainFile i t
  |(st `elem` [D,G])     && (lc==ClassN) = set_str_field old_t i . fromStr $ t
  |otherwise                           = i
  where 
    lc= classification l

setProjectName :: Info -> SC.Token -> Info
setProjectName i t = set_name_in_Info i . fromIdent . fromIK $ t

setMainFile :: Info -> SC.Token -> Info
setMainFile i t = set_main_file_in_Info i . fromStr $ t

set_name::Project_Title->String->Project_Title
set_name p n = p {name = n}

set_main_file::Project_Title->String->Project_Title
set_main_file p m = p {main_file = m}

set_name_in_Info :: Info->String->Info
set_name_in_Info i n = set_project i $ set_name (project i) n

set_main_file_in_Info::Info->String->Info
set_main_file_in_Info i m = set_project i $ set_main_file (project i) m

set_project::Info->Project_Title->Info
set_project i p= i {project = p}

set_compiler::Info->String->Info
set_compiler i p = i {compiler=p}

set_compiler_flags::Info->String->Info
set_compiler_flags i p = i {compiler_flags=p}

set_linker::Info->String->Info
set_linker i p = i {linker=p}

set_linker_flags::Info->String->Info
set_linker_flags i p = i {linker_flags=p}

set_source_dir::Info->String->Info
set_source_dir i p = i {source_dir=p}

set_source_exts::Info->String->Info
set_source_exts i p = i {source_exts=p}

set_build_dir::Info->String->Info
set_build_dir i p = i {build_dir=p}

set_include_dirs::Info->String->Info
set_include_dirs i p =i {include_dirs=p}

set_makefile_name::Info->String->Info
set_makefile_name i p = i {makefile_name=p}

fromIK::SC.Token->IdKeyw
fromIK (SC.IK s) = s
fromIK _         = error "Ожидается ключевое слово или идентификатор."

fromIdent:: IdKeyw->String
fromIdent (Ident s) = s 
fromIdent _         = error "Ожидается идентификатор."

fromStr::SC.Token->String
fromStr (SC.Str s) = s 
fromStr _          = error "Ожидается строковый литерал."

set_str_field::SC.Token->Info->String->Info
set_str_field t i s=
  case t' of
    Compiler       -> set_compiler i s 
    Linker         -> set_linker i s 
    Makefile_name  -> set_makefile_name i s 
    Compiler_flags -> set_compiler_flags i s 
    Linker_flags   -> set_linker_flags i s 
    Source_dir     -> set_source_dir i s 
    Source_exts    -> set_source_exts i s 
    Build_dir      -> set_build_dir i s 
    Include_dirs   -> set_include_dirs i s 
    _              -> i
  where
    t' = fromIK t

prot_lex :: SC.Lexem -> SC.Lexem -> SC.Lexem
prot_lex old_l curr_l =
  if (lc == ClassB) || (lc == ClassC) then curr_l else old_l
  where
    lc = classification curr_l