module StringLiterals(
  stringLiteral
) where

stringLiteral::String->Int->(String,String,String)
stringLiteral sh n =
  (hvst,reverse v,if not b then unexpectedEnd ++ show n ++ "." else "")
  where 
    (hvst,v,b)=stringAccumulate sh 

data StrState=A | B | C
  deriving(Eq,Ord,Show)

stringAccumulate::String->(String,String,Bool)
stringAccumulate xs=stringAccumulate' xs [] A

stringAccumulate'::String->String->StrState->(String,String,Bool)
stringAccumulate' (_:xs) v A = stringAccumulate' xs v B
stringAccumulate' [] _ A = ([],[],False)  
stringAccumulate' [] v B = ([],v,False)
stringAccumulate' (x:xs) v B =
  if x /= '\"' then
    stringAccumulate' xs (x:v) B
  else
    stringAccumulate' xs v C
stringAccumulate' [] v C = ([],v,True)
stringAccumulate' y@(x:xs) v C =
  if x/='\"' then
    (y,v,True)
  else
    stringAccumulate' xs ('\"':v) B

unexpectedEnd::String
unexpectedEnd = "Неожиданное окончание строкового литерала в строке "