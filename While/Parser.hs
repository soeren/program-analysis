-------------------------------------------------------------
-- Parser for our adapted WHILE language
-------------------------------------------------------------

module While.Parser( loadFromFile ) where

import Data.Maybe
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language( javaStyle )

import While.ParserAS

loadFromFile fname
  = do{input <- readFile fname
      ; case parse program fname input of
           Left err -> do {print err; return Nothing}
           Right x  -> return (Just x)
      }

-----------------------------------------------------------
-- A program contains a several variable definitions,
-- several function definitions and several statements
-----------------------------------------------------------
program = do{ many (try comments)
            ; spaces
            ; reserved "program"
            ; defines <- many (try define)
            ; functions <- many func
    	    ; stats <- many1 stat
    	    ; reserved "end"
            ; return (Prog defines functions
                           (if length stats < 2 then head stats else Seq stats))
        }

-- This is used to look for one line or several line comments
comments = choice[try blockComment, lineComment]

blockComment = do{ string "/*"
                 ; t <- manyTill anyChar (try (string "*/"))
                 ; spaces
                 ; return t
                 }

lineComment = do{ string "//"
                ; t <- manyTill anyChar (try newline)
                ; spaces
                ; return t
                }

-- variable definitions
define :: Parser Define
define = do{ vartype <- varType
           ; id <- identifier
           ; semi
           ; return (Define vartype id)
           }

defineArgs = do{ vartype <- varType
               ; id <- identifier
               ; return (Arg vartype id)
               }

-- types of variables in our language
varType = symbol "int"

-- function definition
func :: Parser Func
func = do{ vartype <- varType
         ; name <- identifier
         ; args <- parens (commaSep defineArgs)
         ; symbol "{"
         ; defines <- many (try define)
         ; stats <- many1 stat
         ; symbol "}"
         ; return (Func vartype name args defines
                            (if length stats < 2 then head stats else Seq stats) 0)
       }

-- statement definition
stat :: Parser Stat
stat = choice
       [ skipStat
       , ifStat
       , whileStat
       , sequenceStat
       , try assignStat
       , returnStat
       , writeStat
       , readStat
       ]

-- write definition
writeStat :: Parser Stat
writeStat = do{ reserved "write"
              ; e <- aritExpr
              ; semi
              ; return (Write e 0)
             }

-- read definition
readStat :: Parser Stat
readStat = do{ reserved "read"
             ; id <- identifier
             ; semi
             ; return (Read id 0)
             }

-- return definition
returnStat :: Parser Stat
returnStat = do{ reserved "return"
               ; e <- aritExpr
               ; semi
               ; return (Return e 0)
               }

-- skip definition
skipStat :: Parser Stat
skipStat = do{ reserved "skip"
             ; semi
             ; return (Skip 0)
             }

-- assignment definition
assignStat :: Parser Stat
assignStat = do{ id <- identifier
               ; symbol assignOpName
               ; s <- aritExpr
               ; semi
               ; return (Assign id s 0)
               }

-- if definition
ifStat :: Parser Stat
ifStat = do{ reserved "if"
           ; cond <- parens boolExpr
           ; thenpart <- stat
           ; reserved "else"
           ; elsepart <- stat
           ; return (If cond 0 thenpart elsepart)
           }

-- while definition
whileStat :: Parser Stat
whileStat = do{ reserved "while"
              ; cond <- parens boolExpr
              ; body <- stat
              ; return (While cond 0 body)
              }

-- statement sequence definition
sequenceStat :: Parser Stat
sequenceStat = do{ stats <- braces (many1 stat)
                 ; return (if length stats < 2 then head stats else Seq stats)
                 }

-- function call definition
funccall :: Parser AExp
funccall = do{ name <- identifier
             ; args <- parens (commaSep aritExpr)
             ; return (FuncCall name args)
             }


-----------------------------------------------------------
-- Expressions
-----------------------------------------------------------
boolExpr:: Parser BExp
boolExpr = buildExpressionParser boolOperators boolArgs

boolArgs = choice [ boolLiteral
                  , try relExpr
                  , parens relExpr
                  ]

relExpr :: Parser BExp
relExpr = do{ arg1 <- aritExpr
            ; op <- choice (map (\o -> let s = symbol o in if length o > 1 then try s else s)
                            relOpNames)
            ; arg2 <- aritExpr
            ; return (RelOp op arg1 arg2)
            }

aritExpr :: Parser AExp
aritExpr =  buildExpressionParser aritOperators simpleArit

-- Everything mapping bools to bools
boolOperators = map (\o -> [if o == "!" then prefix o else opbb o AssocRight])
                 boolOpNames
    where
      opbb name = Infix (do{ reservedOp name
                           ; return (BOp name)
                           })
      prefix name = Prefix  (do{ reservedOp name
                                  ; return (BUnOp name)
                                  })

-- Everything mapping pairs of ints to ints
aritOperators = map (\o -> [op o AssocLeft]) aritOpNames
    where
      op name = Infix (do{ reservedOp name
                         ; return (AOp name)
                         })

simpleArit = choice [ intLiteral
                    , parens aritExpr
                    , try funccall
                    , variable
                    ]

boolLiteral = do{ reserved "true"
               ; return (BoolLit True)
               }
             <|>
             do{ reserved "false"
               ; return (BoolLit False)
               }

intLiteral = do{ i <- integer; return (IntLit i) }
variable = do{ id <- identifier
             ; return (Var id)
             }


-----------------------------------------------------------
-- The lexer
-----------------------------------------------------------
lexer     = P.makeTokenParser whileDef

relOpNames = ["<", "<=", ">", ">=", "="]
boolOpNames = ["!", "&", "|"]
aritOpNames = ["*", "+", "-"]
assignOpName = ":="

whileDef  = javaStyle
          {
            P.reservedNames  = [ "true", "false", "if", "else",
                                 "while", "skip", "read", "write",
                                 "program", "end"
                               ]
          , P.reservedOpNames= assignOpName : relOpNames ++ boolOpNames ++ aritOpNames
          , P.opLetter       = oneOf (concat (P.reservedOpNames whileDef))
          , P.caseSensitive  = False
          }

parens          = P.parens lexer
braces          = P.braces lexer
commaSep        = P.commaSep lexer
symbol          = P.symbol lexer
identifier      = P.identifier lexer
reserved        = P.reserved lexer
reservedOp      = P.reservedOp lexer
integer         = P.integer lexer
semi            = P.semi lexer
