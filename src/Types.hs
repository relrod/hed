module Types where

import qualified Data.ByteString.Char8 as B
import qualified Data.Sequence as S

data LineRange = LineRange Int Int deriving (Eq, Ord, Show)

data InputLine =
    PrintLineRange LineRange
  | PrintLineRangeWithNumbers LineRange
  | Print
  | Write
  | WriteFilename String
  | Quit
  | Number Int
  | Append
  | RunCommand String
  | DeleteRange LineRange
  | Delete
  | Change
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
