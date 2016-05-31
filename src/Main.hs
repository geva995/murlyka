module Main where
import MakeContents
import System.Environment

main::IO()
main = do
  args <- getArgs
  mapM_ handleMakefileDescr args