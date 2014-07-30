{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Parser (parseProgram) where

import Text.ParserCombinators.Parsec

import Program

parseProgram :: String -> Either ParseError Program
parseProgram s = parse pProgram "(command line)" s

pProgram :: GenParser Char st Program
pProgram = do char '('
              string "lambda"
              whitespace
              char '('
              x <- many alphaNum
              char ')'
              whitespace
              e <- expression
              char ')'
              return (Program (makeId x) e)

expression :: GenParser Char st Expression
expression =
   (char '0' >> return (E0))
   <|> (char '1' >> return (E1))
   <|> (do char '('; e <- pExpr; char ')'; return e)
   <|> (do x <- many alphaNum ; return (EId (makeId x)))
   <?> "expression"

pExpr :: GenParser Char st Expression
pExpr = pIf <|> pFold <|> pOp1 <|> pOp2 <?> "parentesized expression"

pIf :: GenParser Char st Expression
pIf = do string "if0 "
         e1 <- expression
         whitespace
         e2 <- expression
         whitespace
         e3 <- expression
         return (EIf0 e1 e2 e3)

pFold :: GenParser Char st Expression
pFold = do string "fold "
           e1 <- expression
           whitespace
           e2 <- expression
           whitespace
           string "(lambda ("
           id1 <- many alphaNum
           whitespace
           id2 <- many alphaNum
           char ')'
           whitespace
           e3 <- expression
           char ')'
           return (EFold e1 e2 (makeId id1) (makeId id2) e3)


pOp1 :: GenParser Char st Expression
pOp1 = (do string "not "; e <- expression; return (EOp1 OP_NOT e))
       <|> (do string "sh"; pShift)

pShift :: GenParser Char st Expression
pShift = (do string "l1 "; e <- expression; return (EOp1 OP_SHL1 e))
         <|> (do char 'r'; pRShift)

pRShift :: GenParser Char st Expression
pRShift = (do string "4 "; e <- expression; return (EOp1 OP_SHR4 e))
          <|> try (do string "1 "; e <- expression; return (EOp1 OP_SHR1 e))
          <|> (do string "16 "; e <- expression; return (EOp1 OP_SHR16 e))


pOp2 :: GenParser Char st Expression
pOp2 = (do string "and "; e1 <- expression; whitespace; e2 <- expression; return (EOp2 OP_AND e1 e2))
       <|> (do string "or "; e1 <- expression; whitespace; e2 <- expression; return (EOp2 OP_OR e1 e2))
       <|> (do string "xor "; e1 <- expression; whitespace; e2 <- expression; return (EOp2 OP_XOR e1 e2))
       <|> (do string "plus "; e1 <- expression; whitespace; e2 <- expression; return (EOp2 OP_PLUS e1 e2))


whitespace :: GenParser Char st String
whitespace = many (oneOf " \t")
