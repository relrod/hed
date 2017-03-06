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
