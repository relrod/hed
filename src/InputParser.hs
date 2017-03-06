module InputParser where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as B
import Types

printLineNumber :: Parser InputLine
printLineNumber = do
  first <- read <$> many digit
  return (PrintLineRange (first - 1) (first - 1))

printLineNumberRange :: Parser InputLine
printLineNumberRange = do
  first <- read <$> many digit
  char ','
  second <- read <$> many digit
  return (PrintLineRange (first - 1) (second - 1))

printLines :: Parser InputLine
printLines = (printLineNumberRange <|> printLineNumber) <* char 'p'

printLineNumberWithNumber :: Parser InputLine
printLineNumberWithNumber = do
  first <- read <$> many digit
  return (PrintLineRangeWithNumbers (first - 1) (first - 1))

printLineNumberRangeWithNumbers :: Parser InputLine
printLineNumberRangeWithNumbers = do
  first <- read <$> many digit
  char ','
  second <- read <$> many digit
  return (PrintLineRangeWithNumbers (first - 1) (second - 1))

printLinesWithNumbers :: Parser InputLine
printLinesWithNumbers =
  (printLineNumberRangeWithNumbers <|> printLineNumberWithNumber) <* char 'n'

write :: Parser InputLine
write = char 'w' *> return Write

writeFilename :: Parser InputLine
writeFilename = do
  char 'w'
  many1 (char ' ')
  filename <- many1 (notChar ' ')
  return (WriteFilename filename)

parseInput :: Parser InputLine
parseInput =
  choice [ printLines
         , printLinesWithNumbers
         , writeFilename
         , write
         ]
