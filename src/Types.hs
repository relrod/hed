module Types where

import qualified Data.ByteString.Char8 as B
import qualified Data.Sequence as S

data InputLine =
    PrintLineRange Int Int
  | PrintLineRangeWithNumbers Int Int
  | Print
  | Write
  | WriteFilename String
  | Quit
  | Number Int
  | Append
  deriving (Eq, Ord, Show)

data FileInfo =
  FileInfo {
    filename :: Maybe String
  , contents :: S.Seq B.ByteString
  } deriving (Eq, Ord, Show)

data EditorMode =
    Insert
  | Command
  deriving (Eq, Ord, Show)

data EditorState =
  EditorState {
    editorMode :: EditorMode
  , lineNumber :: Int
  } deriving (Eq, Ord, Show)
