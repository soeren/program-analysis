-- Module for Dead Code Elimination
module Optimizer.DeadCode (deadCode) where


import Data.Maybe
import Data.Set (member)

import While.ParserAS
import DataFlow.Generic (fvS, calledFuncs, labeling, hasChangedP, hasChangedF)
import qualified DataFlow.LV as LV

-- dead code elimination on statements
deadc lv t = case t of
               Assign v _ l -> deadA t v l
               Read v l -> deadA t v l
               If b l s1 s2 -> reduce (If b l (kill s1) (kill s2))
               While b l s -> reduce (While b l (kill s))
               Seq s -> reduce (deadS s)
               _ -> Just t
    where
      deadc' = deadc lv
      -- the basic check of assignment/read is dead
      isDead v l = let d = lookup l lv in
                    isNothing d || not (v `member` fromJust d)
      deadA t v l = if isDead v l then Nothing else Just t
      -- DCE on a sequence
      deadS s = if null s' then Skip 0 else Seq s'
          where
            s' = filter notSkip $ catMaybes $ map deadc' $ deadReturn s
      -- remove dead code after return statemen
      deadReturn (s:ss) = case s of
                            Return _ _ -> [s]
                            _ -> s : deadReturn ss
      deadReturn [] = []
      notSkip (Skip _) =  False
      notSkip _ = True
      -- remove dead branches
      reduce (Seq (s:[])) = Just s
      reduce (Seq []) = Nothing
      reduce (Seq s) = Just (Seq s)
      reduce (If (BoolLit True) _ s1 _) = Just s1
      reduce (If (BoolLit False) _ _ s2) = Just s2
      reduce (If _ _ (Skip _) (Skip _)) = Nothing
      reduce (If b l s1 s2) = Just (If b l s1 s2)
      reduce (While (BoolLit False) _ _) = Nothing
      reduce (While _ _ (Skip _)) = Nothing
      reduce (While b l s) = Just (While b l s)
      reduce s = Just s
      kill = unjust . deadc'

-- replace completely emptied sequences with skip,
-- that is filtered if later if possible
unjust Nothing = Skip 0
unjust (Just s) = s

-- removed all dead variable definitions
deadDef d s = filter (\(Define _ v) -> v `elem` vs) d
    where
      vs = fvS s

-- remove all dead functions
deadFuncs p@(Prog _ f _) = filter (\(Func _ f _ _ _ _) -> f `elem` liveFuncs) f
    where
      liveFuncs = calledFuncs p

-- execute DCE on program:
--   remove dead definitions
--   remove dead functions
--   removed dead variables/labels
-- until no further removal possible
deadCode p@(Prog d _ s) = if hasChangedP p p' then deadCode p' else p
    where
      p' = labeling (Prog dd df ds)
      dd = deadDef d ds
      df = map deadCodeF (deadFuncs p)
      ds = unjust $ deadc (fst $ LV.lv p) s

-- execute DCE on a function
--   remove dead local variable defintions
--   remove dead variables/labels
-- until no further removal possible
deadCodeF f@(Func t i a d s l) = if hasChangedF f f' then deadCodeF f' else f
    where
      f' = Func t i a dd ds l
      dd = deadDef d ds
      ds' = deadc (fst $ LV.lvF f) s
      ds = case ds' of
             -- should never occur if there is at least one return statement
             Nothing -> error $ "Completely dead function: "++i
             _ -> let v = fromJust ds' in
                  case v of
                    Skip _ -> error $ "No return reachable in function: "++i
                    _ -> v
