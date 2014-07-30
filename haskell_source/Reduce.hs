module Reduce (symEval) where

import Program

symEval :: Expression -> Expression
symEval E0 = E0
symEval E1 = E1
symEval e@(EId _) = e
symEval (EOp1 OP_NOT e) = case symEval e of
                           (EOp1 OP_NOT e2) -> e2
                           e' -> (EOp1 OP_NOT e')
symEval (EOp1 OP_SHL1 e) = case symEval e of
                            E0 -> E0
                            e' -> (EOp1 OP_SHL1 e')

-- right shifts in unique order
symEval (EOp1 OP_SHR4 (EOp1 OP_SHR1 e)) = symEval (EOp1 OP_SHR1 (EOp1 OP_SHR4 e))
symEval (EOp1 OP_SHR16 (EOp1 OP_SHR1 e)) = symEval (EOp1 OP_SHR1 (EOp1 OP_SHR16 e))
symEval (EOp1 OP_SHR16 (EOp1 OP_SHR4 e)) = symEval (EOp1 OP_SHR4 (EOp1 OP_SHR16 e))

-- remaining op1 are the right shifts
symEval (EOp1 shr e) = case symEval e of
                        E0 -> E0
                        E1 -> E0
                        (EOp1 OP_NOT E1) -> EOp1 shr (EOp1 OP_NOT E0)
                        (EOp2 OP_AND E1 _) -> E0
                        (EOp2 OP_OR  E1 e') -> EOp1 shr e'
                        (EOp2 OP_XOR E1 e') -> EOp1 shr e'
                        (EIf0 _ E0 E1) -> E0
                        (EIf0 _ E1 E0) -> E0
                        {-
                        (EOp1 OP_SHR1 (EOp1 OP_SHR1 (EOp1 OP_SHR1 e')))
                           | shr == OP_SHR1 -> EOp1 OP_SHR4 e'
                        (EOp1 OP_SHR4 (EOp1 OP_SHR4 (EOp1 OP_SHR4 e')))
                           | shr == OP_SHR4 -> EOp1 OP_SHR16 e'
                        -}
                        e' -> EOp1 shr e'

symEval (EOp2 OP_PLUS e1 e2) =
   case (symEval e1, symEval e2) of
      (E0, e2') -> e2'                                     -- (plus 0 e) = e
      (E1, EOp1 OP_NOT E0) -> E0                           -- (plus 1 (not 0)) = 0
      (E1, EOp1 OP_NOT E1) -> EOp1 OP_NOT E0               -- (plus 1 (not 1)) = (not 0)
      (EOp1 OP_NOT E0, EOp1 OP_NOT E0) -> EOp1 OP_NOT E1   -- (plus (not 0) (not 0)) = (not 1)
      (e1', e2') -> EOp2 OP_PLUS e1' e2'  

symEval (EOp2 OP_AND e1 e2) =
   case (symEval e1, symEval e2) of
      (E0, _) -> E0                         -- (and 0 e) = 0
      (E1, EOp1 OP_NOT E0) -> E1            -- (and 1 (not 0)) = 1
      (E1, e2'@(EOp2 OP_AND E1 _)) -> e2'   -- (and 1 (and 1 e)) = (and 1 e) 
      (EOp1 OP_NOT E0, e2') -> e2'          -- (and (not 0) e) = e
      (e1', EOp1 OP_NOT e2') | e1' == e2' -> E0 -- (and e (not e)) = 0
      (e1', e2') | e1' == e2' -> e1'        -- (and e e) = e
                 | otherwise  -> EOp2 OP_AND e1' e2'

symEval (EOp2 OP_OR e1 e2) =
   case (symEval e1, symEval e2) of
      (E0, e2') -> e2'                       -- (or 0 e) = e
      (E1, e2'@(EOp1 OP_NOT E0)) -> e2'      -- (or 1 (not 0)) = (not 0)
      (E1, EOp1 OP_NOT E1) -> EOp1 OP_NOT E0 -- (or 1 (not 1)) = (not 0)
      (E1, e2'@(EOp2 OP_OR E1 _)) -> e2'     -- (or 1 (or 1 e)) = (or 1 e)
      (e1'@(EOp1 OP_NOT E0), _) -> e1'       -- (or (not 0) e) = (not 0)
      (e1', e2') | e1' == e2' -> e1'         -- (or e e) = e
                 | otherwise  -> EOp2 OP_OR e1' e2'

symEval (EOp2 OP_XOR e1 e2) =
   case (symEval e1, symEval e2) of
      (E0, e2') -> e2'                         -- (xor 0 e) = e
      (E1, EOp1 OP_NOT E0) -> EOp1 OP_NOT E1   -- (xor 1 (not 0)) = (not 1)
      (E1, EOp1 OP_NOT E1) -> EOp1 OP_NOT E0   -- (xor 1 (not 1)) = (not 0)
      (E1, EOp2 OP_XOR E1 e2') -> e2'          -- (xor 1 (xor 1 e)) = e
      (EOp1 OP_NOT E0, e2') -> symEval $ EOp1 OP_NOT e2' -- (xor (not 0) e) = (not e)
      (e1', e2') | e1' == e2' -> E0            -- (xor e e) = 0
                 | otherwise  -> EOp2 OP_XOR e1' e2'

symEval (EIf0 e1 e2 e3) = case symEval e1 of
                            E0 -> symEval e2
                            E1 -> symEval e3
                            (EOp1 OP_NOT E0) -> symEval e3
                            (EOp1 OP_NOT E1) -> symEval e3
                            e1' -> simplifyIf0 e1' (symEval e2) (symEval e3)

symEval (EFold e1 e2 v1 v2 e3) =
  let e3' = symEval e3
      vs = freeVars e3'
  in
     if v1 `notElem` vs && v2 `notElem` vs then
       e3'
     else if v1 `notElem` vs then
       EFold E0 (symEval e2) v1 v2 e3'
     else if v2 `notElem` vs then
       EFold (symEval e1) E0 v1 v2 e3'
     else 
       EFold (symEval e1) (symEval e2) v1 v2 e3'

simplifyIf0 :: Expression -> Expression -> Expression -> Expression
simplifyIf0 e1 e2 e3 | e2 == e3  = e2
                     | e2 == E0 && e1 == e3 = e1
                     | e1 == e2  = symEval $ EIf0 e1 E0 e3
                     | otherwise = EIf0 e1 e2 e3
