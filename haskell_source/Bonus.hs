{-

BONUS Programm generieren

Annahmen:
- immer if0 am Anfang
- immer and
- nie  fold

(lambda (x)
  (if0 (and (plus (shr16 (shr16 (shl1 x))) (shr16 x)) 1)
       (not (plus x (not 1)))
       (and (not 1) (not x))
   ))

(lambda (x)
  (if0 (and (or (shr4 (shr4 x)) x) 1)
       (xor (or x (plus x x)) x)
       (plus (xor x (plus 1 x)) x)
   ))


(lambda (x)
  (if0 (and (xor x (shr4 (and (shl1 x) x))) 1)
       (plus x (or 1 x))
       (and x (plus x 1))
   ))

(lambda (x)
  (if0 (and P 1)
       T
       F))

size = |P| + |T| + |F| + 4

-}

module Bonus (genBonusPrograms) where

import Data.Word

import Program
import Eval
import Generator


genBonusPrograms :: [Op1] -> [Op2] -> Int -> [(Word64,Word64)] -> [Program]
genBonusPrograms ops1 ops2 size samples
   = [prog | (p,e1,e2) <- genFragments ops1 ops2 (size - 2) samples,
             let prog = Program Id0 (EIf0 p e1 e2),
             all (\(inp,out) -> eval prog inp == out) samples]



genFragments :: [Op1] -> [Op2] -> Int -> [(Word64,Word64)] -> [(Expression, Expression, Expression)]
{-
genFragments ops1 ops2 size samples
  = let allExps = genAllExpressions (ops1, ops2, True, False) 0
        upTo n = concat (take n allExps)
        fromTo m n = concat (take (1 + n - m) (drop (m - 1) allExps))
        ofSize n = allExps !! (n - 1)
    in
     [(e1, e2, e3)
      | e2 <- upTo (size - 2), let e2s = exprSize e2,
        e1 <- fromTo 3 (size - 1 - e2s), isAnd1 e1 && occursVar Id0 e1, let e1s = exprSize e1,
        all (satisfyIfA e1 e2) samples,
        let e2s = exprSize e2,
        e3 <- upTo (size - e1s - e2s)]
-}

genFragments ops1 ops2 size samples
  = let allExps = genAllExpressions (ops1, ops2, True, False) 0
        upTo n = concat (take n allExps)
        fromTo m n = concat (take (1 + n - m) (drop (m - 1) allExps))
        ofSize n = allExps !! (n - 1)
    in
     [(p, e1, e2)
      | p <- fromTo 3 (size - 3), isAnd1 p && occursVar Id0 p,
        let ps = exprSize p, 
        e1 <- upTo (size - 1 - ps),
        occursVar Id0 e1 && all (satisfyIfA p e1) samples,
        let e1s = exprSize e1,
        e2 <- upTo (size - ps - e1s),
        all (satisfyIf p e1 e2) samples]


isAnd1 :: Expression -> Bool
isAnd1 (EOp2 OP_AND E1 _) = True
isAnd1 _ = False

satisfyIfA :: Expression -> Expression -> (Word64,Word64) -> Bool
satisfyIfA p e (inp,out) | evalSimpleExpr p inp == 0  = evalSimpleExpr e inp == out
                         | otherwise = True

satisfyIf :: Expression -> Expression -> Expression -> (Word64,Word64) -> Bool
satisfyIf p e1 e2 (inp,out) | evalSimpleExpr p inp == 0  = evalSimpleExpr e1 inp == out
                            | otherwise                  = evalSimpleExpr e2 inp == out


satisfy :: Expression -> Expression -> [(Word64,Word64)] -> Bool
satisfy e1 e2 samples = all (sat1 e1 e2) samples

sat1 :: Expression -> Expression -> (Word64, Word64) -> Bool
sat1 e1 e2 (input,output) = evalSimpleExpr e1 input == output ||
                            evalSimpleExpr e2 input == output
