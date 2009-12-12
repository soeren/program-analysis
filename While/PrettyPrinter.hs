-- Pretty printer module for our framework
-- and our variant of the WHILE language

module While.PrettyPrinter where

import Text.PrettyPrint.HughesPJ

import While.ParserAS

-- pretty print variable definitions
prettyD d = vcat (map prettyD' d)
    where
      prettyD' (Define t i) = text t <+> text i <> semi

-- pretty print a function (head, local var definitions, and body)
prettyF f = vcat (map prettyF' f)
    where
      prettyF' (Func t i a d s l) = headF t i a l $+$ braces (bodyF d s)
      headF t i a l = text t <+> text i <> parens (argF a) <+> lab l
      argF = hcat . punctuate comma . map argF'
      argF' (Arg t i) = text t <+> text i
      bodyF d s = text "" $+$ nest 4 (prettyD d $+$ prettyS s) $+$ text ""

-- pretty print a program or function body (=sequence)
prettyS s = case s of
              Seq s -> vcat (map prettyS s)
              Assign v a l -> text v <+> text ":=" <+> prettyA a <> semi <+> lab l
              Skip l -> text "skip;" <+> lab l
              Read i l -> text "read" <> parens (text i) <> semi <+> lab l
              Write a l -> text "write" <> parens(prettyA a) <> semi <+> lab l
              If b l s1 s2 -> headI b <+> lab l  $+$ braces(body s1 4) <+> text "else" $+$ braces(body s2 4)
              While b l s -> headW b <+> lab l $+$ braces(body s 4)
              Return a l -> text "return" <+> prettyA a <> semi <+> lab l
    where
      -- pretty print While and If headers
      headW b = text "while" <> parens(prettyB b)
      headI b = text "if" <> parens(prettyB b)
      -- pretty print branches
      body s i = text "" $+$ nest i (prettyS s) $+$ text ""
      -- pretty print arithmetic expressions
      prettyA a = case a of
                    Var v -> text v
                    IntLit i ->  integer i
                    AOp o a1 a2 -> prettyA a1 <+> text o <+> prettyA a2
                    FuncCall i a -> text i <> parens(prettyA' a)
      prettyA' = hcat . punctuate comma . map prettyA
      -- pretty print Boolean expressions
      prettyB b = case b of
                    BoolLit b -> prettyB' b
                    BUnOp s b -> text s <> prettyB b
                    BOp s b1 b2 -> prettyB b1 <+> text s <+> prettyB b2
                    RelOp s a1 a2 -> prettyA a1 <+> text s <+> prettyA a2
      prettyB' b = if b then text "true" else text "false"

-- print comment of the label
lab l = text "//" <+> int l

vcat' = foldl ($+$) empty

-- pretty print a complete program parse tree
printPretty (Prog d f s) = render $ vcat' [text "program",
                                           (prettyD d),
                                           (prettyF f),
                                           (prettyS s),
                                           text "end"]
