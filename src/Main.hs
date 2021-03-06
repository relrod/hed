{-# LANGUAGE ViewPatterns #-}
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
  let cts = contents file
  inp <- getLine
  case parseOnly parseInput (B.pack inp) of
    Left err -> putStrLn "?"
    Right (PrintLineRange (U.numericLR cts ->
                           LineRange (LineNumber a) (LineNumber b))) ->
      if a >= 0 && b < S.length cts
      then mapM_ (\x -> B.putStrLn (S.index cts x)) [a..b]
      else putStrLn "?"
    Right (PrintLineRangeWithNumbers
           (U.numericLR cts ->
            LineRange (LineNumber a) (LineNumber b))) ->
      if a >= 0 && b < S.length cts
      then mapM_ (\x ->
                   B.putStrLn ((B.pack . show $ x + 1) <>
                               B.pack "\t" <> (S.index cts x)))
           [a..b]
      else putStrLn "?"
    Right Print ->
      case S.lookup (lineNumber state) cts of
        Nothing -> putStrLn "?" -- Should never happen, maybe?
        Just line -> B.putStrLn line
    Right PrintNumeric ->
      case S.lookup (lineNumber state) cts of
        Nothing -> putStrLn "?" -- Should never happen, maybe?
        Just line ->
          B.putStrLn ((B.pack . show $ lineNumber state + 1) <>
                      B.pack "\t" <> line)
    Right Write ->
      case filename file of
        Nothing -> putStrLn "?"
        Just f -> do
          bytes <- U.writeFile file
          print bytes
          loop file (state { promptSave = False })
    Right (WriteFilename f) -> do
      bytes <- U.writeFile (file { filename = Just f })
      print bytes
      loop file (state { promptSave = False })
    Right Quit ->
      if promptSave state
      then putStrLn "?" >> loop file (state { promptSave = False })
      else exitSuccess
    Right (Number (U.numericL cts -> n)) ->
      case S.lookup n cts of
        Nothing -> putStrLn "?"
        Just line -> do
          B.putStrLn line
          loop file (state { lineNumber = n })
    Right Append -> do
      newLines <- U.grabMultiline
      let contents' = U.insertSeqAt cts (lineNumber state + 1) newLines
          file' = file { contents = contents' }
          state' = state { lineNumber = lineNumber state + S.length newLines
                         , editorMode = Command
                         , promptSave = True
                         }
      loop file' state'
    Right (RunCommand cmd) -> callCommand cmd >> putStrLn "!"
    Right (DeleteRange linerange) -> do
      let contents' = U.deleteSeqRange cts linerange
          file' = file { contents = contents' }
      loop file' (state { promptSave = True })
    Right Delete -> do
      let line = LineRange
                 (LineNumber $ lineNumber state)
                 (LineNumber $ lineNumber state)
          contents' = U.deleteSeqRange cts line
          file' = file { contents = contents' }
          state' = state { promptSave = True }
      loop file' state'
    Right Change -> do
      let x = S.deleteAt (lineNumber state) cts
      newLines <- U.grabMultiline
      let contents' = U.insertSeqAt x (lineNumber state) newLines
          file' = file { contents = contents' }
          state' = state { promptSave = True }
      loop file' state'
  loop file state

initialState :: EditorState
initialState = EditorState Command 0 False

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
