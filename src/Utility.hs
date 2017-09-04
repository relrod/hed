{-# LANGUAGE ViewPatterns #-}
module Utility where

import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int64)
import qualified Data.Sequence as S
import Types

writeFile :: FileInfo -> IO (Maybe Int64)
writeFile file =
  let withNewlines = S.intersperse (B.pack "\n") (contents file)
      folded = foldMap BB.byteString withNewlines
      toBL = BB.toLazyByteString folded
  in
    case filename file of
      Nothing -> return Nothing
      Just f -> BL.writeFile f toBL >> return (Just (BL.length toBL))

insertSeqAt :: S.Seq a -> Int -> S.Seq a -> S.Seq a
insertSeqAt orig idx new =
  case S.viewl new of
    S.EmptyL -> orig
    a S.:< new' ->
      insertSeqAt (S.insertAt idx a orig) (idx + 1) new'

grabMultiline :: IO (S.Seq B.ByteString)
grabMultiline = grabMultiline' (S.fromList [])
  where
    grabMultiline' acc = do
      inp <- B.getLine
      if inp == B.pack "."
        then return acc
        else grabMultiline' (acc S.|> inp)

deleteSeqRange :: S.Seq a -> LineRange -> S.Seq a
deleteSeqRange s (numericLR s -> LineRange (LineNumber a) (LineNumber b))
  | b < a = s
  | otherwise =
    deleteSeqRange (S.deleteAt a s) (LineRange (LineNumber a) (LineNumber (b - 1)))

numericLR :: S.Seq a -> LineRange -> LineRange
numericLR f (LineRange EndOfFile n@(LineNumber _)) =
  LineRange (LineNumber $ S.length f - 1) n
numericLR f (LineRange n@(LineNumber _) EndOfFile) =
  LineRange n (LineNumber $ S.length f - 1)
numericLR f (LineRange EndOfFile EndOfFile) =
  LineRange (LineNumber $ S.length f - 1) (LineNumber $ S.length f - 1)
numericLR f (LineRange a b) = LineRange a b

numericL :: S.Seq a -> LineNumber -> Int
numericL f (LineNumber n) = n - 1
numericL f EndOfFile = S.length f - 1
