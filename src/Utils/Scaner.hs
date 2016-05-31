module Scaner(
  scaner,
  Token(..),
  Lexem
) where
import qualified Data.List as Lists
import qualified Data.Char as Chars
import IdKeywords
import Delimiters
import StringLiterals
import UnknownLexem

data Token = None | Unknown | IK IdKeyw | D Delims | Str String
  deriving(Eq,Ord,Show)

getToken'::String->Int->(String,Token,String)
getToken' [] _ = ([],None,"")
getToken' y@(x:_) n
  | x `elem` ['(',')'] = (\(h,d)->(h,D d,"")) $ delim y
  | x=='\"'            = (\(h,v,m)->(h,Str v,m)) $ stringLiteral y n
  | is_id_char x       = (\(h,v)->(h,IK v,"")) $ idKeyword y 
  | otherwise          = (\h -> (h,Unknown,unknown_msg n)) $ unknownLexem y

getToken :: String->Int->(String,Token,String)
getToken s n = getToken' (dropWhile Chars.isSpace s) n

unknown_msg :: Int -> String
unknown_msg n = "Нераспознаваемая лексема в строке " ++ show n

type Lexem = (Token,Int)

lineSkaner::String->Int->[(Token,String)]
lineSkaner xs n = reverse $ lineSkaner' xs n []

lineSkaner' :: String->Int->[(Token,String)]->[(Token,String)]
lineSkaner' xs n ts =
  if t ==None then 
    ts
  else 
    lineSkaner' hvst n $ (t,diagnose):ts
  where
    (hvst,t,diagnose) = getToken xs n

scaner :: String->Either String [Lexem]
scaner xs = 
  if not . null $ tm then
    Left tm
  else
    Right tl
  where 
    y = reverse . snd . Lists.foldl' ( \(n, ls) s -> (n+1, (s, n):ls) ) (1, []) $ lines xs
    w = map convertTokenList $ map (\(s, n) -> (lineSkaner s n, n) ) y
    (tl,tm)=(\(ls',ss)->(concat ls', unlines . filter (not . null) . concat  $ ss)) $ unzip w 

convertTokenList :: ([(Token, String)], Int) -> ([Lexem], [String])
convertTokenList (tss, n) = (reverse lexems, reverse d)
  where
    (lexems, d) = Lists.foldl' (\(ls, strs) (t, s) -> ((t, n):ls, s:strs) ) ([], []) tss