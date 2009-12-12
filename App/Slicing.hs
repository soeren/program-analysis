-- Program Slicing
module App.Slicing (slice) where

import Data.Maybe
import qualified Data.Set as S
import Data.List (nub)

import While.ParserAS
import DataFlow.Generic (getBlock, getLabel, scopeFunc, getFunc,
                         callGraph, validLabel, calledFuncsSNonGreedy)
import qualified DataFlow.RD as RD

-- Checks if pi is a valid label of program p
-- If so it performs the slicing, otherwise returns the empty list
slice p pi = if validLabel p pi
             then S.toList $ progSlice' p Nothing pi
             else []

-- Performs the program slice for the scope the pi is located
-- in. Return set of highlighted labels
progSlice' p@(Prog _ _ s) mask pi = S.insert pi slice
    where
      -- Perform actual program slicing
      slice = pSlice p func (fst rd) s' pi S.empty mask

      -- Find name of function the PI is located in
      fname = scopeFunc p pi

      -- Obtain function given by its name 'fname'
      func = if isJust fname then getFunc p $ fromJust fname else Nothing

      -- Perform RD analysis on scope: either function or program
      (rd, s') = rdScope func
      rdScope Nothing = (RD.rd p, s)
      rdScope (Just fn@(Func _ _ _ _ s _)) = (RD.rdF fn, s)

-- should never happen
pSlice _ Nothing _ _ pi _ _ | pi < 0 = S.empty

-- inter-procedural slicing
pSlice p (Just f) _ _ pi _ _ | pi < 0 = S.unions ps'
                             where
                               -- Perform program slicing for every
                               -- label the function is called
                               ps' = map (\(f, l) -> progSlice' p (Just (l, (f, pi))) l) callers

                               -- Obtain callgraph and filter out only the function we are interested in
                               callers = filter (\(f', _) -> fn == f') $ callGraph p
                               fn = getFuncName f
                               getFuncName (Func _ f _ _ _ _) = f

-- intra-procedural/program slicing
pSlice p fn rd s pi l mask = if isNothing b' then S.empty
                             else S.union h' $ S.unions . S.toList $
                             S.map (\i -> pSlice p fn rd s i (S.union l h) mask) h
    where
      -- Filter out negative labels (due to dependency on arguments). Should not occur in final program slice
      h' = S.filter ((<) 0) h

      -- Filter out duplicates, so we do not analyse labels twice
      h = S.filter (not . (`S.member` l)) $ S.unions [parentLabels, relVarsLabels, relFuncs]

      -- Obtain labels of the scope (if/while)
      parentLabels = S.fromList $ scopeLabels pi s

      -- Relevant variable tuples from RD and their corresponding labels
      relVarsLabels = S.map snd relVarsTuples
      relVarsTuples = S.filter (\(x, _) -> (x `elem` vs)) $ fromJust $ lookup pi rd

      -- Relevant function calls
      relFuncs = S.fromList $ relFuncLabels p fs

      -- Relevant vars occuring in PI block 'b'
      vs = if maskMatch mask then relVars mask b else relVars Nothing b

      -- Relevant function calls occuring in PI block 'b'
      fs = if maskMatch mask then relFuncCalls p mask b else relFuncCalls p Nothing b
      b' = getBlock pi s
      b = fromJust b'
      maskMatch Nothing = False
      maskMatch (Just m) = pi == fst m

-- Obtain relevant function calls and it labels
relFuncLabels p = map fl . catMaybes . map (getFunc p)
    where
      fl (Func _ _ _ _ _ l) = l
relFuncCalls p Nothing = nub . map fst . calledFuncsSNonGreedy p []
relFuncCalls p (Just (_, (f, _))) = filter (f ==) . relFuncCalls p Nothing

-- Returns the relevant variables of a statement
-- It navigates through the statment tree, concatenating the
-- relevant variables
relVars mask s = case s of
                   Assign _ a _ -> varsA mask a
                   Write a _ -> varsA mask a
                   Return a _ -> varsA mask a
                   If b _ _ _ -> varsB mask b
                   While b _ _ -> varsB mask b
                   _ -> []
    where
      -- Obtain variables in arithmetic epxression, without mask applied
      varsA Nothing t = case t of
                          Var n -> [n]
                          AOp _ a1 a2 -> varsA mask a1 ++ varsA mask a2
                          FuncCall _ as  -> concatMap (varsA mask) as
                          _ -> []

      -- Obtain variables in arith. expr., with mask applied.
      -- Mask applies to function calls and its arguments
      varsA (Just (_, (f, i))) t = case t of
                                     FuncCall f' as | f == f' -> concatMap (varsArg i) $
                                                                 zip [-1,-2..] as
                                     AOp _ a1 a2 -> varsA mask a1 ++ varsA mask a2
                                     _ -> []

      -- Obtain variables for relevant arguments
      varsArg i a = if i == fst a then varsA Nothing $ snd a else []

      -- Obtain variables in boolean expression
      varsB mask t = case t of
                       BUnOp _ e -> varsB mask e
                       BOp _ e1 e2 -> varsB mask e1 ++ varsB mask e2
                       RelOp _ e1 e2 -> varsA mask e1 ++ varsA mask e2
                       _ -> []

-- In case the statement is inside one or several if/while,
-- it returns the concatenation of the labels of each of those
scopeLabels l s = case s of
                    Seq s -> concatMap (scopeLabels l) s
                    _ | l == getLabel s -> [l]
                    If _ l' s1 s2 -> scopeLabels' l' s1 ++ scopeLabels' l' s2
                    While _ l' s' -> scopeLabels' l' s'
                    _ -> []
    where
      scopeLabels' l' s' = let x = scopeLabels l s' in if null x then [] else l' : x
