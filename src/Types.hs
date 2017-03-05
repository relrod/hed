module Types where

data InputLine =
    PrintLineRange Int Int
  | PrintLineRangeWithNumbers Int Int
  deriving (Eq, Ord, Show)
