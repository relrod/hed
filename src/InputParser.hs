module InputParser where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as B
import Types

lineNumber' :: Parser LineRange
lineNumber' = do
  first <- read <$> many digit
  return (LineRange (first - 1) (first - 1))

lineNumberRange :: Parser LineRange
lineNumberRange = do
  first <- read <$> many digit
  char ','
  second <- read <$> many digit
  return (LineRange (first - 1) (second - 1))

printLines :: Parser InputLine
printLines = PrintLineRange <$> (lineNumberRange <|> lineNumber') <* char 'p'

printLinesWithNumbers :: Parser InputLine
printLinesWithNumbers =
  PrintLineRangeWithNumbers <$> (lineNumberRange <|> lineNumber') <* char 'n'

deleteLines :: Parser InputLine
deleteLines =
  DeleteRange <$> (lineNumberRange <|> lineNumber') <* char 'd'

deleteCurrent :: Parser InputLine
deleteCurrent = char 'd' *> return Delete

printCurrent :: Parser InputLine
printCurrent = char 'p' *> return Print

write :: Parser InputLine
write = char 'w' *> return Write

writeFilename :: Parser InputLine
writeFilename = do
  char 'w'
  many1 (char ' ')
  filename <- many1 (notChar ' ')
  return (WriteFilename filename)

runCommand :: Parser InputLine
runCommand = do
  char '!'
  cmd <- many1 anyChar
  return (RunCommand cmd)

quit :: Parser InputLine
quit = char 'q' *> return Quit

changeLine :: Parser InputLine
changeLine = do
  nr <- read <$> many1 digit
  return (Number (nr - 1))

append :: Parser InputLine
append = char 'a' *> return Append

change :: Parser InputLine
change = char 'c' *> return Change

parseInput :: Parser InputLine
parseInput =
  choice [ printCurrent
         , deleteCurrent
         , printLines
         , printLinesWithNumbers
         , deleteLines
         , changeLine
         , writeFilename
         , runCommand
         , write
         , quit
         , append
         , change
         ]
