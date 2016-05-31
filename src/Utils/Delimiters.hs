module Delimiters(
  Delims(..),
  delim
) where 

data Delims = LP | RP
  deriving (Eq,Ord,Show)

delim::String->(String,Delims)
delim xs =
  case head xs of
    '(' -> (t,LP)
    _   -> (t,RP)
  where
    t = tail xs