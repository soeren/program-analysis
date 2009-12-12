-- Constant Propagation Analysis using MFP
module DataFlow.CP (cp, cpF, cpFa, Z' (Const)) where

import Data.Maybe

import DataFlow.MFP
import DataFlow.Generic (flow, init', vars, getBlock, args, getFunc)
import While.ParserAS

data Z' a = Bottom | Const a | Top
            deriving (Eq, Show)

-- Partial Order for Z'
instance (Eq a) => Ord (Z' a) where
    (Const c1) <= (Const c2) = c1 == c2
    (Const _) <= Top = True
    Bottom <= (Const _) = True
    Top <= Top = True
    Top <= _ = False
    (Const _) <= Bottom = False
    Bottom <= Top = True
    Bottom <= Bottom = True

-- Partial order for State
partialOrder l1 l2 = all isOrder l1
    where
      isOrder (v, z) = let z' = lookup v l2 in
                       isJust z' && z <= fromJust z'

-- Union operator for Z' values
union c@(Const c1) (Const c2) = if c1 /= c2 then Top else c
union c@(Const _) Bottom = c
union Bottom c@(Const _) = c
union Bottom Bottom = Bottom
union Top _ = Top
union _ Top = Top

-- Compute least upper bound for two subsets of lattice
leastUpper l1 l2 = map union' l1
    where
      union' (v, z) = (v, union z (lookup' v l2))
      lookup' v l = let z' = lookup v l in
                    if isJust z' then fromJust z' else Bottom

-- Initialize CP state with all given variables set to 'z'
initState z = map (\v -> (v, z))

cp' p i b scope s = mfp s f e i b fl o1 o2
    where
      f = flow s -- forward analysis
      e = init' s -- initial labels as extremal labels

      -- operators for the MFP algorithm
      o1 = partialOrder
      o2 = leastUpper

      -- transfer function
      fl l = fl' (getBlock l s)
      fl' Nothing sig = sig
      fl' (Just b) sig = case b of
                    Assign v a _ -> if isBottomState sig then sig
                                    else setState sig v (acp sig a)
                    Read v _ -> setState sig v Top
                    _ -> sig

      -- In a bottom state all variables have value Bottom
      isBottomState = all (\(_, z) -> z == Bottom)

      -- Set value 'z' for variable 'v' in state 'sig'
      setState sig v z = case x of
                           Nothing -> sig
                           Just _ -> (v, z) : filter (\(v', _) -> v /= v') sig
          where
            x = lookup v sig

      -- Compute constantness of arithmetic expression
      acp _ (IntLit i) = Const i
      acp sig (Var x) = fromJust $ lookup x sig
      acp sig (AOp o a1 a2) = op o (acp sig a1) (acp sig a2)
      acp sig (FuncCall f as) = if f /= scope
                                then constFunc f $ map (acp sig) as
                                else Top

      -- Apply arithmetic operator, only actually performed if both
      -- operands are constant
      op _ Bottom _ = Bottom
      op _ _ Bottom = Bottom
      op _ Top _ = Top
      op _ _ Top = Top
      op o (Const i1) (Const i2) = Const (o' i1 i2)
          where
            o' = fromJust $ lookup o ops
            ops = [("+", (+)), ("-", (-)), ("*", (*))]

      -- Compute constant value for function 'fname' called with
      -- arguments 'a'
      constFunc fname a = foldl union Bottom rs
          where
            f = fromJust $ getFunc p fname
            getFuncS (Func _ _ _ _ s _) = s
            rs = returnVals (fst $ cpFa' p f fname a) (getFuncS f)

      -- Obtain all values of type Z' appearing in return statements
      returnVals cp t = case t of
                          Return a l -> [acp (fromJust $ lookup l cp) a]
                          If _ _ s1 s2 -> rv s1 ++ rv s2
                          While _ _ s -> rv s
                          Seq s -> concatMap rv s
                          _ -> []
          where
            rv = returnVals cp

-- CP for Program
cp p@(Prog d _ s) = cp' p i b "" s
    where
      i = initState Top $ vars d
      b = initState Bottom $ vars d

-- CP for Function
cpF p (Func _ f a d s _) = cp' p i b f s
    where
      i = initState Top (vars d ++ args a)
      b = initState Bottom (vars d ++ args a)

-- CP for functions with values of type Z' assigned for arguments
cpFa' p (Func _ _ a d s _) scope vs = cp' p i b scope s
    where
      i = initState Top (vars d) ++ zip (args a) vs
      b = initState Bottom (vars d) ++ zip (args a) vs

-- CP for functions with ordinary values assigned for arguments
-- Only IntLit arguments are assumed to be constant
cpFa p (Func _ _ a d s _) scope vs = cp' p i b scope s
    where
      i = initState Top (vars d) ++ zip (args a) vs'
      b = initState Bottom (vars d) ++ zip (args a) vs'
      vs' = map fromIntLit vs
      fromIntLit (IntLit i) = Const i
      fromIntLit _ = Top
