{-# LANGUAGE GADTs #-}
module JoinBlock (joinBlock) where

import IR hiding (body)
import Compiler.Hoopl

joinBlock :: Graph Insn C C -> Graph Insn C C
joinBlock graph = loop graph labels
    where labels = singleRef $ countLabel graph
          loop :: Graph Insn C C -> [Label] -> Graph Insn C C
          loop = foldl (flip joinAndRemoveBlock)

type LabelCount = LabelMap Bool
countLabel :: Graph Insn C C -> LabelCount
countLabel graph = foldGraphNodes f graph mapEmpty
    where f :: Insn e x -> LabelCount -> LabelCount
          f (Jump label) m = mapInsert label (case mapLookup label m of
                                                Nothing -> True
                                                _ -> False) m
          f (Cond _ l1 l2) m = mapInsert l1 False (mapInsert l2 False m)
          f _ m = m

singleRef :: LabelCount -> [Label]
singleRef lc = map fst $ mapToList $ mapFilter (== True) lc

joinAndRemoveBlock :: Label -> Graph Insn C C -> Graph Insn C C
joinAndRemoveBlock label (GMany pre body aft) =
          let (preLabel, blk1) = findSubstBlock label body
              blk2 = case mapLookup label body of Just blk -> blk; _ -> error "some error"
              body1 = mapDelete preLabel body
              body2 = mapDelete label body1
              (h1, _) = blockSplitTail blk1
              (_, t1) = blockSplitHead blk2
              newblk = blockAppend h1 t1
              body3 = mapInsert preLabel newblk body2
            in GMany pre body3 aft

findSubstBlock :: Label -> Body' Block Insn -> (Label, Block Insn C C)
findSubstBlock label body = head $ filter help $ mapToList body
    where help (_, blk) = case lastNode blk of
                            Jump lb -> lb == label
                            _ -> False
