-- Reaching definitions analysis module
-- uses MFP to implement the analysis
module DataFlow.RD (rd, rdF) where

import Data.Set hiding (map)

import DataFlow.Generic (getBlock, flow, init', vars)
import DataFlow.MFP
import While.ParserAS

-- Traverse the statement tree to obtain a list of (variable, label)
-- tuples, where the given variable v was assigned a value.
getAssigns v s = case s of
                   If _ _ s1 s2 -> getAssigns' [s1, s2]
                   While _ _ s -> getAssigns v s
                   Seq s -> getAssigns' s
                   Assign v' _ l | v == v' -> [(v, l)]
                   Read v' l | v == v' -> [(v, l)]
                   _ -> []
    where
      getAssigns' = concatMap (getAssigns v)

rd' s i = mfp s f e i b fl o1 o2
    where
      f = flow s -- forwards analysis
      e = init' s -- init(program) as extremal label
      b = empty -- bottom value for unanalyzed labels
      -- transfer function
      fl l x = difference x (kill s b) `union` gen b l where b = getBlock l s
      -- operators for the MFP algorithm
      o1 = isSubsetOf
      o2 = union
      -- kill and gen functions for the RD analysis
      kill _ Nothing = empty
      kill s (Just b) = case b of
                          Assign v _ _ -> fromList ((v, 0) : getAssigns v s)
                          Read v _ -> fromList ((v, 0) : getAssigns v s)
                          _ -> empty
      gen Nothing _ = empty
      gen (Just b) l = case b of
                           Assign v _ _ -> singleton (v, l)
                           Read v _ -> singleton (v, l)
                           _ -> empty

-- RD analysis on functions
rdF (Func _ _ a d s _) = rd' s i
    where
      i = fromList (map (\v -> (v, 0)) (vars d) ++
                    zip (map (\(Arg _ v) -> v) a) [-1,-2..])

-- RD analysis on a program
rd (Prog d _ s) = rd' s i
    where
      i = fromList (map (\v -> (v, 0)) (vars d))
