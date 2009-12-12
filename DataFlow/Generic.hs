-- Generic module
-- exports common functions used by the other modules
module DataFlow.Generic
    (labeling, labels, flow, flowR, final, init', vars, getBlock,
    getLabel, fvS, fvA, fvB, calledFuncs, getSizeF,
    callGraph, scopeFunc, getFunc, args, validLabel,
    hasChangedP, hasChangedF, calledFuncsSNonGreedy)
where

import Data.List (find, nub)
import Data.Maybe
import qualified Data.Set as S

import While.ParserAS

-- returns the number of labels required for a construct/sequence
getSize t = case t of
             If _ _ s1 s2 -> 1 + getSize s1 + getSize s2
             While _ _ s -> 1 + getSize s
             Seq s -> sum $ map getSize s
             _ -> 1

-- returns the number of labels required for a function
getSizeF (Func _ _ _ _ ss _) = 1 + getSize ss

-- label all blocks in a program including the functions
labeling (Prog d f s) = Prog d lf ls
    where
      ls = labelingS s 1
      lf = fst $ foldl labelingFunc' ([], 1 + getSize ls) f

-- aux. methods to label a list of functions. Required to know the
-- number of labels used in the previously labeled function.
labelingFunc' (lf, l) f = (lf ++ [f'], l + getSizeF f')
    where
      f' = labelingFunc f l

-- Label statements in a function
labelingFunc (Func t f as ds ss _) l = Func t f as ds (labelingS ss (l + 1)) l

-- aux. method to traverse all blocks of a statement and label them
labelingS t l = case t of
                 Assign v a _ -> Assign v a l
                 Skip _ -> Skip l
                 Read v _ -> Read v l
                 Write a _ -> Write a l
                 Return a _ -> Return a l
                 If b _ s1 s2 -> If b l (labelingS s1 (l + 1)) (labelingS s2 (l + getSize s1 + 1))
                 While b _ s -> While b l (labelingS s (l + 1))
                 Seq s -> Seq (labeling' s l)
                     where labeling' [] _ = []
                           labeling' (s:ss) l = labelingS s l : labeling' ss (l + getSize s)

-- returns the label of a statement/head of constructs or sequence
getLabel s = case s of
               Assign _ _ l -> l
               Skip l -> l
               Read _ l -> l
               Write _ l -> l
               If _ l _ _ -> l
               While _ l _ -> l
               Return _ l -> l
               Seq s -> getLabel (head s)

-- returns list of tuple of passed integer and label of passed statement
getLabelPair l s = [(l, getLabel s)]

-- returns list of all labels in a statement/sequence
labels t = case t of
             If _ _ s1 s2 -> getLabel t : labels s1 ++ labels s2
             While _ _ s -> getLabel t : labels s
             Seq s -> concatMap labels s
             _ -> [getLabel t]

-- Returns a tuple of inner-flows and ending labels. For example, an
-- if construct has at least two end labels, one for each branch. And
-- the inner flow is the combination of the flows for both branches.
flow'' s = case s of
            If _ l s1 s2 -> (f, l1 ++ l2)
                where
                  f = getLabelPair l s1 ++ f1 ++ getLabelPair l s2 ++ f2
                  (f1, l1) = flow' s1
                  (f2, l2) = flow' s2
            While _ l s' -> (f, [l])
                 where
                   f = getLabelPair l s' ++ f' ++ map (\x -> (x, l)) l'
                   (f', l') = flow' s'
            _ -> ([], [getLabel s])

-- The given flow f' and the endling labels l' are connected with the
-- statement s. The new flow is the concatenation of f', the tuples of
-- the endling labels with the label of s, and the inner flow of
-- s. The new endling labels are the ones of s.
connect (f', l') s = (f' ++ map (\x -> (x, getLabel s)) l' ++ f, l)
    where
      (f, l) = flow'' s

-- In case of a sequence, connect all the inner-flows and ending
-- labels for each sequence with the neighbouring ones. Otherwise just
-- return the flow and ending labels for t.
flow' t = case t of
           Seq (s:ss) -> foldl connect (flow'' s) ss
           _ -> flow'' t

-- The flow is just the first value in the tuple returned by flow'
flow = fst . flow'

-- Reverse all tuples in the flow
flowR = map (\(x, y) -> (y, x)) . flow

-- Returns the ending labels of a program
final = snd . flow'

-- returns the initial label of a program
init' t = [getLabel t]

-- get a list of all defined variables of the definition part
vars = map (\(Define _ n) -> n)

-- get a list all arguments of a function
args = map (\(Arg _ a) -> a)

-- get block with the label l
getBlock l s = case s of
                 Seq s -> getBlock' l s
                 _ | l == getLabel s -> Just s
                 If _ _ s1 s2 -> getBlock' l [s1, s2]
                 While _ _ s -> getBlock l s
                 _ -> Nothing
    where
      pmap f = catMaybes . map f
      getBlock' l = find (const True) . pmap (getBlock l)

-- get list of all variable names used in arithmetic expression a
fvA a = case a of
          Var v -> [v]
          AOp _ a1 a2 -> concatMap fvA [a1, a2]
          FuncCall _ as -> concatMap fvA as
          IntLit _ -> []

-- get list of all variable names used in Boolean expression b
fvB b = case b of
          BUnOp _ b -> fvB b
          BOp _ b1 b2 -> concatMap fvB [b1, b2]
          RelOp _ a1 a2 -> concatMap fvA [a1, a2]
          BoolLit _ -> []

-- get list of all variable names used in statement s
fvS s = case s of
         Assign v a _ -> v : fvA a
         If b _ s1 s2 -> fvB b ++ concatMap fvS [s1, s2]
         While b _ s' -> fvB b ++ fvS s'
         Seq s' -> concatMap fvS s'
         Read v _ -> [v]
         Write a _ -> fvA a
         Return a _ -> fvA a
         Skip _ -> []

-- get the function with name "name"
getFunc (Prog _ f _) name = find (\(Func _ f' _ _ _ _) -> f' == name) f

-- get list of all called functions
calledFuncs = nub . map fst . callGraph

-- function to create the callgraph of a program
callGraph p@(Prog _ _ s) = calledFuncsS p [] s

-- aux. method that returns list of all functions called within a function
calledFuncsF l p cf (Func _ f _ _ s _) = if f' `elem` cf then [] else f' : calledFuncsS p (f':cf) s
    where
      f' = (f, l)

-- aux. method that returns list of all functions called within a
-- arithmetic expression
calledFuncsA p cf a l = case a of
                          FuncCall f as -> let
                                   f' = getFunc p f
                                   cf' = if isJust f'
                                         then calledFuncsF l p cf $ fromJust f'
                                         else [(f, l)] in
                                           cf' ++ map' calledFuncsA p (cf ++ cf') as l
                          AOp _ a1 a2 -> map' calledFuncsA p cf [a1, a2] l
                          _ -> []

-- In order to analyse a list of artihmetic/boolean expressions or
-- statements, we have to consider the list of called functions
-- obtained from the analysis of the previous arithmetic
-- expression. Otherwise, we might end up in an endless loop. This is
-- done by using a foldl, carrying the list of called functions from
-- one analysis to the next.
map' c p cf xs l = foldl (\a x -> a ++ c p (a ++ cf) x l) [] xs
mapS c p cf = foldl (\a x -> a ++ c p (a ++ cf) x) []

-- aux. method that returns list of all functions called within a
-- Boolean expression
calledFuncsB p cf b l = case b of
                          BUnOp _ b -> calledFuncsB p cf b l
                          BOp _ b1 b2 -> map' calledFuncsB p cf [b1, b2] l
                          RelOp _ a1 a2 -> map' calledFuncsA p cf [a1, a2] l
                          BoolLit _ -> []

-- aux. method that returns list of all functions called within a statement
calledFuncsS p cf s = case s of
                        If b _ s1 s2 -> let cf' = calledFuncsB p cf b l in
                                        cf' ++ mapS calledFuncsS p (cf ++ cf') [s1, s2]
                        While b _ s -> let cf' = calledFuncsB p cf b l in
                                       cf' ++ calledFuncsS p (cf' ++ cf) s
                        Seq s -> mapS calledFuncsS p cf s
                        _ -> calledFuncsSNonGreedy p cf s
    where
      l = getLabel s

-- calledFuncS in non-greedy version, i.e. don't follow the statements
-- in an if/while construct
calledFuncsSNonGreedy p cf s = case s of
                                 Assign _ a _ -> calledFuncsA p cf a l
                                 Write a _ -> calledFuncsA p cf a l
                                 Return a _ -> calledFuncsA p cf a l
                                 If b _ _ _ -> calledFuncsB p cf b l
                                 While b _ _ -> calledFuncsB p cf b l
                                 Seq s -> mapS calledFuncsS p cf s
                                 _ -> []
    where
      l = getLabel s

-- range of labels occupied by a function
funcRanges = map (\(Func _ f _ _ s l) -> (f, (l, l + getSize s)))

-- Obtain the function the label 'l' is located in
scopeFunc (Prog _ f _) l = if isJust f' then Just $ fst $ fromJust f' else Nothing
    where
      rs = funcRanges f
      f' = find (\(_, (l1, l2)) -> l1 <= l && l <= l2) rs

-- check if a label is valid (i.e. there exists a block with this label)
validLabel p@(Prog _ _ s) l = l `elem` (labels s ++ funcL f)
    where
      fname = scopeFunc p l
      f = if isJust fname then getFunc p $ fromJust fname else Nothing
      funcL Nothing = []
      funcL (Just (Func _ _ _ _ s _)) = labels s

-- compare the parse trees of two functions including their local
-- definitions for differences
hasChangedF (Func _ _ _ d s _) (Func _ _ _ d' s' _) = cd || cs
    where
      cd = S.fromList (vars d) /= S.fromList (vars d')
      cs = getSize s /= getSize s'

-- compare the parse trees of two programs including their functions
-- and definitions for differences
hasChangedP (Prog d f s) (Prog d' f' s') = cd || cs || cf
    where
      cd = S.fromList (vars d) /= S.fromList (vars d')
      cs = getSize s /= getSize s'
      cf = f /= f' || any (uncurry hasChangedF) (zip f f')
