module Main where

import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as B
import Data.Monoid ((<>))
import System.Environment (getArgs)
import System.IO

import InputParser
import Types

loop :: [B.ByteString] -> IO ()
loop file = do
  inp <- getLine
  case parseOnly parseInput (B.pack inp) of
    Left err -> putStrLn "?"
    Right (PrintLineRange a b) ->
      mapM_ (\x -> B.putStrLn (file !! x)) [a..b]
    Right (PrintLineRangeWithNumbers a b) ->
      mapM_ (\x ->
               B.putStrLn ((B.pack . show $ x + 1) <>
                           B.pack "\t" <> (file !! x))) [a..b]
  loop file

main :: IO ()
main = do
  args <- getArgs
  let filename = head args
  file <- B.lines <$> B.readFile filename
  loop file
