module DataFlow.MFP (mfp) where

import Data.List
import Data.Maybe

import DataFlow.Generic (labels)

-- End of recursion, return analysis result
mfp' [] a _ _ _ _ = a

-- Recursive MFP working list algorithm
-- Takes working list, analysis result, flow, transform function,
-- partial order operator, and least upper bound.
mfp' ((l, l'):ws) a f fl o1 o2 =
    if not $ fl l al `o1` al'
    then
        mfp'
        -- Add tuples affected by the current label to the working
        -- list
        (filter (\(l, _) -> l == l') f ++ ws)
        -- Update the analysis entry for l'
        (up a l' $ al' `o2` fl l al)
        f fl o1 o2
    else
        -- Discard head of working list and continue recursion
        mfp' ws a f fl o1 o2
    where
      -- Obtain value for label l from analysis
      al = fromJust $ lookup l a
      -- Obtain value for label l' from analysis
      al' = fromJust $ lookup l' a
      -- Helper function to update entry for label l with value v in
      -- analysis a
      up a l v = (l, v) : delete (fromJust $ find (\(l', _) -> l == l') a) a

-- High-level MFP function
-- Returns tuple with entry and exit analysis
mfp s f e i b fl o1 o2 = (m, map' fl m)
    where
      m = mfp' f a f fl o1 o2
      -- Initialize analysis
      a = map (\x -> if x `elem` e then (x, i) else (x, b)) (labels s)
      map' f = map (\(i, e) -> (i, f i e))
