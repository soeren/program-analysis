-- Live variable analysis module
-- uses MFP to implement the analysis

module DataFlow.LV (lv, lvF) where

import Data.Set hiding (map)

import DataFlow.MFP
import DataFlow.Generic (getBlock, flowR, final, fvA, fvB)
import While.ParserAS

-- traverse the parse tree to get the live variables in each label
lv' s = mfp s f e i b fl o1 o2
    where
      f = flowR s -- backwards analysis
      e = final s -- final(program) as extremal label
      i = empty -- empty set as extremal value
      b = empty -- bottom value for unanalyzed labels
      -- transfer function
      fl l x = difference x (kill b) `union` gen b where b = getBlock l s
      -- operators for the MFP algorithm
      o1 = isSubsetOf
      o2 = union
      -- kill and gen functions for the LV analysis
      kill Nothing = empty
      kill (Just s') = case s' of
                          Assign v _ _ -> singleton v
                          Read v _ -> singleton v
                          _ -> empty
      gen Nothing = empty
      gen (Just s') = case s' of
                           Assign _ a _ -> fromList (fvA a)
                           Return a _ -> fromList (fvA a)
                           Write a _ -> fromList (fvA a)
                           If b _ _ _ -> fromList (fvB b)
                           While b _ _ -> fromList (fvB b)
                           _ -> empty

-- LV analysis on the program body
lv (Prog _ _ s) = lv' s

-- LV analysis on a function body
lvF (Func _ _ _ _ s _) = lv' s
