module Generator (genPrograms, genAllExpressions) where

import Program
import Reduce
import Reduce2

-- generate all programs of size n using at most the given operators
genPrograms :: [Op1] -> [Op2] -> (Bool,Bool,Bool) -> Int -> [Program]
genPrograms ops1 ops2 (if0,fold,tfold) n
  | tfold && n < 6 = error "tfold: size too small"
  | tfold =
     [Program Id0 (EFold (EId (genVar 0)) (E0) Id1 Id2 e)
      | e <- genExpressions (ops1,ops2,if0,fold) 2 (n - 5)]
  | otherwise =
     [Program Id0 e | e <- genExpressions (ops1,ops2,if0,fold) 0 (n - 1)]

genExpressions :: ([Op1], [Op2], Bool,Bool) -> Int -> Int -> [Expression]
genExpressions ops vars size = concat (take size (genAllExpressions ops vars))

genAllExpressions :: ([Op1], [Op2], Bool, Bool) -> Int -> [[Expression]]
genAllExpressions ops@(ops1,ops2,if0,fold) vars = exprs
  where
    exprs = (E0 : E1 : [EId (genVar n) | n <- [0 .. vars]]) : filterTrivial (h 2)
    size n = exprs !! (n - 1)
    upTo n = concat (take n exprs)
    h n = ([EOp1 op e | op <- ops1, e <- size (n - 1)]
           ++
           -- all op2 are commutative, so we can order the expr sizes, e1 <= e2
           [EOp2 op e1 e2 | op <- ops2, e1 <- upTo ((n - 1) `quot`2),
                            let e1size = exprSize e1,
                            let e2size = n - 1 - e1size,
                            e2 <- size (e2size),
                            if e1size == e2size then e1 <= e2 else True]
           ++
           (if if0 && n >= 4
             then [EIf0 e1 e2 e3 | e1 <- upTo (n - 3), let e1size = exprSize e1,
                                   e2 <- upTo (n - 2 - e1size), let e2size = exprSize e2,
                                   e3 <- size (n - 1 - e1size - e2size)]
             else [])
           ++
           (if fold && n >= 5
             then let es = genAllExpressions ops (vars + 2) in
                  [EFold e1 e2 (genVar (vars + 1)) (genVar (vars + 2)) e3
                   | e1 <- upTo (n - 4),          not (occursFold e1), let e1size = exprSize e1,
                     e2 <- upTo (n - 3 - e1size), not (occursFold e2), let e2size = exprSize e2,
                     e3 <- es !! (n - 3 - e1size - e2size),   not (occursFold e3)]
             else []))
          : h (n + 1)

occursFold :: Expression -> Bool
occursFold (EOp1 _ e) = occursFold e
occursFold (EOp2 _ e1 e2) = occursFold e1 || occursFold e2
occursFold (EFold _ _ _ _ _) = True
occursFold (EIf0 e1 e2 e3) = occursFold e1 || occursFold e2 || occursFold e3
occursFold _ = False


filterTrivial :: [[Expression]] -> [[Expression]]
filterTrivial ess = map (filter (not . canBeSimplified)) ess
-- filterTrivial = id

canBeSimplified :: Expression -> Bool
canBeSimplified e = symEval e /= e || simplify e /= e
