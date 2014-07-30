module Eval (eval, evalSimpleExpr) where

import Data.Word
import Data.Bits

import Program

eval :: Program -> Word64 -> Word64
eval (Program var e) value = evalExpr (newBindings var value) e

evalSimpleExpr :: Expression -> Word64 -> Word64
evalSimpleExpr e input = evalExpr (newBindings Id0 input) e

evalExpr :: Bindings -> Expression -> Word64
evalExpr _  E0 = 0
evalExpr _  E1 = 1
evalExpr bs (EId var) = lookupBinding bs var
evalExpr bs (EIf0 e1 e2 e3) = if evalExpr bs e1 == 0
                                then evalExpr bs e2
                                else evalExpr bs e3
evalExpr bs (EOp1 op e) = op1Apply op (evalExpr bs e)
evalExpr bs (EOp2 op e1 e2) = op2Apply op (evalExpr bs e1) (evalExpr bs e2)
evalExpr bs (EFold wExpr zExpr y z e) =
  evalFold bs (evalExpr bs wExpr) (evalExpr bs zExpr) y z e

evalFold :: Bindings -> Word64 -> Word64 -> Id -> Id -> Expression -> Word64
evalFold bs word zero x y e = h 0 zero
  where
    h n z | n > 7 = z
          | otherwise = h (n + 1) (evalExpr
                                     (bind2 x ((word `shiftR` (8 * n)) .&. 255)
                                            y z bs)
                                     e)

op1Apply :: Op1 -> Word64 -> Word64
op1Apply OP_NOT v = complement v
op1Apply OP_SHL1 v = shiftL v 1
op1Apply OP_SHR1 v = shiftR v 1
op1Apply OP_SHR4 v = shiftR v 4
op1Apply OP_SHR16 v = shiftR v 16

op2Apply :: Op2 -> Word64 -> Word64 -> Word64
op2Apply OP_PLUS a b = a + b
op2Apply OP_AND a b = a .&. b
op2Apply OP_OR a b = a .|. b
op2Apply OP_XOR a b = xor a b

data Bindings = BNil
              | B !Id !Word64 !Bindings

newBindings :: Id -> Word64 -> Bindings
newBindings var val = bind var val BNil

bind2 :: Id -> Word64 -> Id -> Word64 -> Bindings -> Bindings
bind2 v1 x1 v2 x2 bs = bind v1 x1 (bind v2 x2 bs)

bind :: Id -> Word64 -> Bindings -> Bindings
bind var val bs = B var val bs

lookupBinding :: Bindings -> Id -> Word64
lookupBinding bnds v = case h bnds of
                        Just val -> val
                        Nothing -> error ("unbound identifier: " ++ show v)
  where
    h BNil = Nothing
    h (B var val bs) | v == var = Just val
                     | otherwise = h bs
