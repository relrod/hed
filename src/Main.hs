module Main where

import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as B
import Data.Monoid
import qualified Data.Sequence as S
import System.Environment (getArgs)
import System.Exit
import System.IO
import System.Process

import InputParser
import Types
import qualified Utility as U

loop :: FileInfo -> EditorState -> IO ()
loop file state = do
  inp <- getLine
  case parseOnly parseInput (B.pack inp) of
    Left err -> putStrLn "?"
    Right (PrintLineRange a b) ->
      mapM_ (\x -> B.putStrLn (S.index (contents file) x)) [a..b]
    Right (PrintLineRangeWithNumbers a b) ->
      mapM_ (\x ->
               B.putStrLn ((B.pack . show $ x + 1) <>
                           B.pack "\t" <> (S.index (contents file) x))) [a..b]
    Right Print ->
      case S.lookup (lineNumber state) (contents file) of
        Nothing -> putStrLn "?" -- Should never happen, maybe?
        Just line -> B.putStrLn line
    Right Write ->
      case filename file of
        Nothing -> putStrLn "?"
        Just f -> U.writeFile file >>= print
    Right (WriteFilename f) ->
      U.writeFile (file { filename = Just f }) >>= print
    Right Quit -> exitSuccess -- TODO: ? on unsaved changes
    Right (Number n) ->
      case S.lookup n (contents file) of
        Nothing -> putStrLn "?"
        Just line -> do
          B.putStrLn line
          loop file (state { lineNumber = n })
    Right Append -> do
      newLines <- U.grabMultiline
      let contents' = U.insertSeqAt (contents file) (lineNumber state) newLines
          file' = file { contents = contents' }
          state' = state { lineNumber = lineNumber state + S.length newLines - 1
                         , editorMode = Command
                         }
      loop file' state'
    Right (RunCommand cmd) -> callCommand cmd >> putStrLn "!"
  loop file state

initialState :: EditorState
initialState = EditorState Command 0

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
  loop (FileInfo filename file') initialState
