-- Module for Constant Folding
module Optimizer.ConstFold (constFold) where

import Data.Maybe
import Data.List (nub)

import While.ParserAS
import DataFlow.Generic (getFunc)
import qualified DataFlow.CP as CP

-- Apply arithmetic operator for two constants
opA o (IntLit i1) (IntLit i2) = IntLit (o' i1 i2)
    where
      o' = fromJust $ lookup o ops
      ops = [("+", (+)), ("-", (-)), ("*", (*))]

-- If operands are not constant, return AOp statement
opA o a1 a2 = AOp o a1 a2

-- Apply relation operator for two constants
opR o (IntLit i1) (IntLit i2) = BoolLit (o' i1 i2)
    where
      o' = fromJust $ lookup o ops
      ops = [("<", (<)), ("<=", (<=)), (">", (>)), (">=", (>=)), ("=", (==))]

-- If operands are not constant, return RelOp statement
opR o a1 a2 = RelOp o a1 a2

-- Apply boolean operator for two constants
opB o (BoolLit b1) (BoolLit b2) = BoolLit (o' b1 b2)
    where
      o' = fromJust $ lookup o ops
      ops = [("&", (&&)), ("|", (||))]

-- If operands are not constant, return BOp statement
opB o b1 b2 = BOp o b1 b2

-- Apply unary boolean operator for constant operand
opB' o (BoolLit b) = BoolLit (o' b)
    where
      o' = fromJust $ lookup o ops
      ops = [("!", not)]

-- If operand is not constant, return BunOp statement
opB' o b = BUnOp o b

-- Recursive constant fold function
-- 'p' is program to work on, cp is CP analysis, scope is currently
-- cfolded function (if applicable, otherwise empty), t is current
-- statement in program/function
cfold p cp scope t = case t of
                       Assign v a l -> Assign v (cfoldA l a) l
                       Return a l -> Return (cfoldA l a) l
                       Write a l -> Write (cfoldA l a) l
                       If b l s1 s2 -> If (cfoldB l b) l (cfold' s1) (cfold' s2)
                       While b l s -> While (cfoldB l b) l (cfold' s)
                       Seq s -> Seq (map cfold' s)
                       _ -> t
    where
      cfold' = cfold p cp scope

      -- Fold arithmetic expression 'a' at label 'l'
      cfoldA l a = case a of
                     Var v -> tryGetConst l v
                     AOp o a1 a2 -> opA o (cfoldA l a1) (cfoldA l a2)
                     FuncCall f _ | scope /= f -> tryGetConstF l a -- only fold if not recursive call
                     _ -> a

      -- Fold boolean expression 'b' at label 'l'
      cfoldB l b = case b of
                     BUnOp o b -> opB' o (cfoldB l b)
                     BOp o b1 b2 -> opB o (cfoldB l b1) (cfoldB l b2)
                     RelOp o a1 a2 -> opR o (cfoldA l a1) (cfoldA l a2)
                     _ -> b

      -- Try to return constant value for variable 'v' at label
      -- 'l'. If not constant, return variable
      tryGetConst l v = let cs = lookup l cp in
                        if isNothing cs then Var v
                        else
                            let c = lookup v (fromJust cs) in
                            if isJust c then case fromJust c of
                                               (CP.Const c') -> IntLit c'
                                               _-> Var v
                            else Var v

      -- Try to return constant value for function call. If not
      -- constant, return function call with folded arguments
      tryGetConstF l (FuncCall f as) = if isJust c then fromJust c
                                       else FuncCall f cas
          where
            cas = map (cfoldA l) as
            c = cfoldFunc p f cas
      tryGetConstF _ a = a

-- If returns of function are const, whole function is constant
cfoldFunc p fname a = if all isIntLit rs && length (nub rs) == 1
                      then Just (head rs) else Nothing
    where
      f = fromJust $ getFunc p fname
      getFuncS (Func _ _ _ _ s _) = s
      rs = returnVals $ getFuncS $ constFoldFa p f fname a
      isIntLit (IntLit _) = True
      isIntLit _ = False

-- Return list of values from all return statements
returnVals t = case t of
                 Return a _ -> [a]
                 If _ _ s1 s2 -> returnVals s1 ++ returnVals s2
                 While _ _ s -> returnVals s
                 Seq s -> concatMap returnVals s
                 _ -> []

-- Constant fold function with given arguments
constFoldFa p f@(Func t f' a d s l) scope v = Func t f' a d cs l
    where
      cs = cfold p (fst $ CP.cpFa p f scope v) scope s

-- Constant fold function
constFoldF p f@(Func t f' a d s l) = Func t f' a d cs l
    where
      cs = cfold p (fst $ CP.cpF p f) f' s

-- Constant fold program
constFold p@(Prog d f s) = Prog d cf cs
    where
      cs = cfold p (fst (CP.cp p)) "" s
      cf = map (constFoldF p) f
