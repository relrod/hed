module Main where

import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as B
import Data.Monoid
import qualified Data.Sequence as S
import System.Environment (getArgs)
import System.IO

import InputParser
import Types
import qualified Utility as U

loop :: FileInfo -> IO ()
loop file = do
  inp <- getLine
  case parseOnly parseInput (B.pack inp) of
    Left err -> putStrLn "?"
    Right (PrintLineRange a b) ->
      mapM_ (\x -> B.putStrLn (S.index (contents file) x)) [a..b]
    Right (PrintLineRangeWithNumbers a b) ->
      mapM_ (\x ->
               B.putStrLn ((B.pack . show $ x + 1) <>
                           B.pack "\t" <> (S.index (contents file) x))) [a..b]
    Right Write ->
      case filename file of
        Nothing -> putStrLn "?"
        Just f -> U.writeFile file >>= print
    Right (WriteFilename f) ->
      U.writeFile (file { filename = Just f }) >>= print
  loop file

main :: IO ()
main = do
  args <- getArgs
  let filename = if length args > 0
                 then Just (head args)
                 else Nothing
  file <- case filename of
    Just f -> B.readFile f
    Nothing -> return mempty
  print (B.length file)
  let file' = S.fromList . B.lines $ file
  loop (FileInfo filename file')
