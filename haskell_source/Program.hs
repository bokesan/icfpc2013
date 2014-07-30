module Program (Id(Id0, Id1, Id2), Op1(..), Op2(..), Expression(..), Program(..),
                genVar, makeId, isShiftR, progSize, exprSize, freeVars, occursVar)
where

data Id = Id0 | Id1 | Id2 | Id !String deriving (Eq, Ord)

genVar :: Int -> Id
genVar 0 = Id0
genVar 1 = Id1
genVar 2 = Id2
genVar n = Id ('x' : show n)

-- have to do this to make Eq work!
makeId :: String -> Id
makeId "x0" = Id0
makeId "x1" = Id1
makeId "x2" = Id2
makeId s = Id s

instance Show Id where
    showsPrec _ Id0 = showString "x0"
    showsPrec _ Id1 = showString "x1"
    showsPrec _ Id2 = showString "x2"
    showsPrec _ (Id s) = showString s

data Program = Program !Id !Expression

instance Show Program where
    showsPrec _ (Program x e) = showString "(lambda ("
                                . shows x . showString ") "
                                . shows e . showChar ')'

data Op1 = OP_NOT | OP_SHL1 | OP_SHR1 | OP_SHR4 | OP_SHR16 deriving (Eq, Ord)
data Op2 = OP_AND | OP_OR | OP_XOR | OP_PLUS deriving (Eq, Ord)

instance Show Op1 where
   showsPrec _ op = showString (op1Name op)

instance Show Op2 where
   showsPrec _ op = showString (op2Name op)

isShiftR :: Op1 -> Bool
isShiftR OP_SHR1  = True
isShiftR OP_SHR4  = True
isShiftR OP_SHR16 = True
isShiftR _  = False

op1Name :: Op1 -> String
op1Name OP_NOT = "not"
op1Name OP_SHL1 = "shl1"
op1Name OP_SHR1 = "shr1"
op1Name OP_SHR4 = "shr4"
op1Name OP_SHR16 = "shr16"

op2Name :: Op2 -> String
op2Name OP_AND = "and"
op2Name OP_OR = "or"
op2Name OP_XOR = "xor"
op2Name OP_PLUS = "plus"

data Expression = E0 | E1
                | EId !Id
                | EIf0 Expression Expression Expression
                | EFold Expression Expression !Id !Id Expression
                | EOp1 !Op1 Expression
                | EOp2 !Op2 Expression Expression
                deriving (Eq, Ord)

-- Free variables (approximation only - not correct for fold)
freeVars :: Expression -> [Id]
freeVars expr = h expr []
  where
    h E0 r = r
    h E1 r = r
    h (EId v) r    = v : r
    h (EIf0 e1 e2 e3) r = h e1 $ h e2 $ h e3 r
    h (EFold e1 e2 _ _ e3) r = h e1 $ h e2 $ h e3 r
    h (EOp1 _ e) r = h e r
    h (EOp2 _ e1 e2) r = h e1 $ h e2 r

occursVar :: Id -> Expression -> Bool
occursVar v e = v `elem` freeVars e


progSize :: Program -> Int
progSize (Program _ e) = 1 + exprSize e

exprSize :: Expression -> Int
exprSize expr = expr `seq` h expr 0
  where
    h e c = c `seq` case e of
      E0 -> c + 1
      E1 -> c + 1
      (EId _)    -> c + 1
      (EOp1 _ e1) -> h e1 $ c + 1
      (EOp2 _ e1 e2) -> h e1 $ h e2 $ c + 1
      (EIf0 e1 e2 e3) -> h e1 $ h e2 $ h e3 $ c + 1
      (EFold e1 e2 _ _ e3) -> h e1 $ h e2 $ h e3 $ c + 2

instance Show Expression where
    showsPrec _ E0 = showChar '0'
    showsPrec _ E1 = showChar '1'
    showsPrec _ (EId x)    = shows x
    showsPrec _ (EIf0 e1 e2 e3) = showString "(if0 " . shows e1
                                . showChar ' ' . shows e2
		                . showChar ' ' . shows e3 . showChar ')'
    showsPrec _ (EFold e1 e2 x y e3) =
	   showString "(fold " . shows e1 . showChar ' ' . shows e2
	   . showString " (lambda (" . shows x . showChar ' ' . shows y
	   . showString ") " . shows e3 . showString "))"
    showsPrec _ (EOp1 op e) = showChar '(' . showString (op1Name op)
	                    . showChar ' ' . shows e . showChar ')'
    showsPrec _ (EOp2 op e1 e2) = showChar '(' . showString (op2Name op)
                                . showChar ' ' . shows e1
	                        . showChar ' ' . shows e2 . showChar ')'

