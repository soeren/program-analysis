-- Sanity Checks
module App.Sanity (sane) where

import qualified Data.Set as S
import Data.Maybe

import DataFlow.Generic (vars, args, fvS, fvA, calledFuncs)
import DataFlow.RD
import While.ParserAS

-- All used variables defined?
varsDef' d s = S.isSubsetOf (S.fromList $ fvS s) (S.fromList d)
varsDef (Prog d f s) = varsDef' (vars d) s &&
                       all (\(Func _ _ a d s _) -> varsDef' (args a ++ vars d) s) f

-- Missing defined variables
missVarDef' d s = S.difference (S.fromList $ fvS s) (S.fromList d)
missVarDefF (Func _ f a d s _) = S.map (\v -> f ++ ":" ++ v) $ missVarDef' (args a ++ vars d) s
missVarDef (Prog d f s) = S.toList $ S.union (missVarDef' (vars d) s) (S.unions $ map missVarDefF f)

missVarDefErr p = "The following variables are not defined:\n" ++ show (missVarDef p)

-- Contain 't' a certain statement indicated by True return of isStat
hasStat isStat t = isStat t || case t of
                                 If _ _ s1 s2 -> hasStat' s1 || hasStat' s2
                                 While _ _ s -> hasStat' s
                                 Seq s -> any hasStat' s
                                 _ -> False
    where
      hasStat' = hasStat isStat

isReturn (Return _ _) = True
isReturn _ = False

hasReturn = hasStat isReturn

-- Returns invalid if in program body or no return in any function
invalidReturns (Prog _ f s) = hasReturn s || any (not . hasReturn . getFuncS) f

getFuncS (Func _ _ _ _ s _) = s

retErr _ = "Found return in program body or no return in a function"

isWrite (Write _ _) = True
isWrite _ = False

hasWrite = hasStat isWrite

-- Fails if no write within program or any function contains a write statement
writeProg (Prog _ f s) = not (hasWrite s) || any (hasWrite . getFuncS) f

writeErr _ = "Program body should contain write statement and no write in functions"

isRead (Read _ _) = True
isRead _ = False

hasRead = hasStat isRead

-- Fails if any function contains a read statement
readFuncs (Prog _ f _) = any (hasRead . getFuncS) f

readErr _ = "No read statement within functions"

-- Find uninitialized variables using RD
initLook rd l v = fromJust $ lookup v $ S.toList $ fromJust $ lookup l rd
isInit rd l v = initLook rd l v /= 0
isInit' rd = all . isInit rd

-- Var uninitialized if RD for label 'l' contains a (v, 0)
notInit rd l = map fst . filter ((==) 0 . snd) . map (\v -> (v, initLook rd l v))

-- Traverse s to find a statement with usage of uninitialized variables
uninit rd s = case s of
                 Assign _ a l -> uninit' a l
                 Write a l -> uninit' a l
                 Return a l -> uninit' a l
                 If _ _ s1 s2 -> uninit rd s1 ++ uninit rd s2
                 While _ _ s -> uninit rd s
                 Seq s -> concatMap (uninit rd) s
                 _ -> []
    where
      uninit' a l = if isInit' rd l $ fvA a then [] else [(l, notInit rd l $ fvA a)]

-- Find uninitialized vars in a function
uninitF f@(Func _ _ _ _ s _) = uninit (fst $ rdF f) s

-- Find unitialized vars in a program
uninitP p@(Prog _ f s) = uninit (fst $ rd p) s ++ concatMap uninitF f

-- Fails if program contains usage of uninitalized vars
uninitVars = not . null . uninitP
uninitVarsErr p = "Usage of uninitialized variables:\n" ++
                  unlines (map (\(l, vs) -> "Label " ++ show l ++ " with variables: " ++ show vs)
                           $ uninitP p)

-- Return tuple of called and declared functions
funcsDef' p@(Prog _ f _) = (cf, df)
    where
      cf = calledFuncs p
      df = map getFn f
      getFn (Func _ fn _ _ _ _) = fn

-- List functions called, but not declared
missFuncDef p = S.toList $ S.difference (S.fromList cf) (S.fromList df)
    where
      (cf, df) = funcsDef' p

-- All called functions declared?
funcsDef p = S.isSubsetOf (S.fromList cf) (S.fromList df)
    where
      (cf, df) = funcsDef' p

missFuncDefErr p = "Undefined functions called:\n" ++ show (missFuncDef p)

-- Execute all defined sanity checks and return errors if check failed
sane p = mapMaybe (\(c, e) -> if c p then Just (e p) else Nothing) checkers
    where
      checkers = [(not . varsDef, missVarDefErr),
                  (not . funcsDef, missFuncDefErr),
                  (invalidReturns, retErr),
                  (writeProg, writeErr),
                  (readFuncs, readErr),
                  (uninitVars, uninitVarsErr)
                 ]
