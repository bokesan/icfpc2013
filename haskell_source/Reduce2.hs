{-# LANGUAGE BangPatterns #-}

-- Use interval arithmetic to determine possible values
-- and simplify expressions using that info.

module Reduce2 (simplify) where

-- import Debug.Trace

import Data.Word
import Data.Bits

import Program

data AExpr a = AExpr !a (AExpr' a)

instance Show a => Show (AExpr a) where
    showsPrec _ (AExpr a e) = showChar '[' . shows a . showChar ']' . shows e

getAnn :: AExpr a -> a
getAnn (AExpr a _) = a

data AExpr' a = A0 | A1
              | AId !Id
              | AOp1 !Op1 (AExpr a)
              | AOp2 !Op2 (AExpr a) (AExpr a)
              | AIf0 (AExpr a) (AExpr a) (AExpr a)
              | AFold (AExpr a) (AExpr a) !Id !Id (AExpr a)

instance Show a => Show (AExpr' a) where
   showsPrec _ A0 = showChar '0'
   showsPrec _ A1 = showChar '1'
   showsPrec _ (AId c) = shows c
   showsPrec _ (AOp1 op e) = showChar '(' . shows op . showChar ' ' . shows e . showChar ')'
   showsPrec _ (AOp2 op e1 e2) = showChar '(' . shows op . showChar ' ' . shows e1 . showChar ' ' . shows e2 . showChar ')'
   showsPrec _ (AIf0 e1 e2 e3) = showString "(if0 " . shows e1 . showChar ' ' . shows e2 . showChar ' ' . shows e3 . showChar ')'
   showsPrec _ (AFold e1 e2 v1 v2 e3) = showString "(fold " . shows e1 . showChar ' ' . shows e2
                                        . showString " (lambda (" . shows v1 . showChar ' ' . shows v2
                                        . showString ") " . shows e3 . showChar ')'

values :: Expression -> AExpr Values
values expr = ann [] expr
  where
    ann _   E0 = AExpr (vConst 0) A0
    ann _   E1 = AExpr (vConst 1) A1
    ann env (EId v) = case lookup v env of
                        Just val -> AExpr val (AId v)
                        _ -> AExpr vAny (AId v)    -- forgot why I am getting this. There was a reason...
    ann env (EOp1 op e) = case ann env e of
                            e'@(AExpr a _) -> AExpr (apply1 op a) (AOp1 op e')
    ann env (EOp2 op e1 e2) = case (ann env e1, ann env e2) of
                                (g1@(AExpr a1 _), g2@(AExpr a2 _)) -> AExpr (apply2 op a1 a2) (AOp2 op g1 g2)
    ann env (EIf0 e1 e2 e3)
       = case (ann env e1, ann env e2, ann env e3) of
           (g1@(AExpr a1 _), g2@(AExpr a2 _), g3@(AExpr a3 _))
             -> AExpr (if a1 .==. 0 then a2
                       else if a1 .>. 0 then a3
                       else vUnion a2 a3)
                      (AIf0 g1 g2 g3)

    ann env (EFold e1 e2 v1 v2 e3)
       = let g1@(AExpr word _) = ann env e1
             g2@(AExpr ini  _) = ann env e2
             g3@(AExpr a _)    = ann ((v1, foldVal word) : (v2, vAny) : env) e3
             -- get range for e2 applied to init
             (AExpr a' _) = ann ((v1, foldVal word) : (v2, ini) : env) e3
             -- if range not changed, keep it; otherwise range from any
             a'' = if a' == ini then ini else a
         in AExpr a'' (AFold g1 g2 v1 v2 g3)

-- -------------------------------------------------------------------------------
-- Range of possible values of an expression

data Values = Between {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64
              deriving (Eq)

instance Show Values where
  showsPrec _ (Between a b) | a == 0 && b == maxBound = showChar '?'
                            | a == b  = shows a
                            | a == 0  =  showChar '<' . shows (b+1)
                            | b == maxBound = showChar '>' . shows (a - 1)
                            | otherwise = shows a . showChar '-' . shows b

vAny :: Values
vAny =  Between 0 maxBound

vConst :: Word64 -> Values
vConst x = Between x x

vUnion :: Values -> Values -> Values
vUnion (Between lo1 hi1) (Between lo2 hi2) = Between (min lo1 lo2) (max hi1 hi2)

foldVal :: Values -> Values
foldVal (Between _ hi) = Between 0 (min hi 255)

(.==.) :: Values -> Word64 -> Bool
Between lo hi .==. n  =  lo == n && hi == n

(.>.) :: Values -> Word64 -> Bool
Between lo _ .>. n  =  lo > n

(.<.) :: Values -> Word64 -> Bool
Between _ hi .<. n  =  hi < n

-- -------------------------------------------------------------------------------



apply1 :: Op1 -> Values -> Values
apply1 OP_NOT (Between a b) = Between (complement b) (complement a)
apply1 OP_SHL1 (Between a b) = Between (satShl a) (satShl b)
apply1 OP_SHR1 (Between lo hi) = Between (shiftR lo 1) (shiftR hi 1)
apply1 OP_SHR4 (Between lo hi) = Between (shiftR lo 4) (shiftR hi 4)
apply1 OP_SHR16 (Between lo hi) = Between (shiftR lo 16) (shiftR hi 16)

apply2 :: Op2 -> Values -> Values -> Values

-- Code for addition from Hacker's Delight, figure 4-1
apply2 OP_PLUS (Between a b) (Between c d) = let s = a + c
                                                 t = b + d
                                             in if s >= a && t < b then vAny
                                                else Between s t
apply2 OP_XOR (Between a b) (Between c d) = -- Between 0 (satPlus b d)
                                            Between (minXOR a b c d) (maxXOR a b c d)
apply2 OP_OR  (Between a b) (Between c d) = Between (minOR a b c d) (maxOR a b c d)
apply2 OP_AND (Between a b) (Between c d) = -- Between 0 (min b d)
                                            Between (minAND a b c d) (maxAND a b c d)
satPlus :: Word64 -> Word64 -> Word64
satPlus a b | a > maxBound - b = maxBound
            | otherwise        = a + b

satShl :: Word64 -> Word64
satShl a | a > maxBound - a = maxBound - 1
         | otherwise = a + a

-- more formulas from Hacker's Delight, pages 58ff
minOR :: Word64 -> Word64 -> Word64 -> Word64 -> Word64
minOR !a !b !c !d = h 0x8000000000000000
  where
    h 0 = a .|. c
    h m | (complement a .&. c .&. m) /= 0  = let temp = (a .|. m) .&. (- m)
                                             in if temp <= b then temp .|. c
                                                else h (shiftR m 1)
        | (a .&. complement c .&. m) /= 0  = let temp = (c .|. m) .&. (- m)
                                             in if temp <= d then a .|. temp
                                                else h (shiftR m 1)
        | otherwise = h (shiftR m 1)

maxOR :: Word64 -> Word64 -> Word64 -> Word64 -> Word64
maxOR !a !b !c !d = h 0x8000000000000000
  where
    h 0 = b .|. d
    h m | (b .&. d .&. m) /= 0 =
            let t1 = (b - m) .|. (m - 1) in
            if t1 >= a then t1 .|. d
            else let t2 = (d - m) .|. (m - 1) in
                 if t2 >= c then b .|. t2
                 else h (shiftR m 1)
        | otherwise = h (shiftR m 1)

minAND :: Word64 -> Word64 -> Word64 -> Word64 -> Word64
minAND !a !b !c !d = h 0x8000000000000000
  where
    h 0 = a .&. c
    h m | (complement a .&. complement c .&. m) /= 0 =
            let t1 = (a .|. m) .&. (- m) in
            if t1 <= b then t1 .&. c
            else let t2 = (c .|. m) .&. (- m) in
                 if t2 <= d then a .&. t2
                 else h (shiftR m 1)
        | otherwise = h (shiftR m 1)

maxAND :: Word64 -> Word64 -> Word64 -> Word64 -> Word64
maxAND !a !b !c !d = h 0x8000000000000000
  where
    h 0 = b .&. d
    h m | (b .&. complement d .&. m) /= 0  = let temp = (b .&. complement m) .|. (m - 1)
                                             in if temp >= a then temp .&. d
                                                else h (shiftR m 1)
        | (complement b .&. d .&. m) /= 0  = let temp = (d .&. complement m) .|. (m - 1)
                                             in if temp >= c then b .&. temp
                                                else h (shiftR m 1)
        | otherwise = h (shiftR m 1)

minXOR, maxXOR :: Word64 -> Word64 -> Word64 -> Word64 -> Word64
minXOR !a !b !c !d = minAND a b (complement d) (complement c) .|.
                     minAND (complement b) (complement a) c d
maxXOR !a !b !c !d = maxOR 0 (maxAND a b (complement d) (complement c))
                           0 (maxAND (complement b) (complement a) c d)

-- --------------------------------------------------------

simplify :: Expression -> Expression
-- simplify e = let v = values e in simpl $ trace ("ANN: " ++ show v) v
simplify e = simpl $ values e

simpl :: AExpr Values -> Expression
simp :: AExpr' Values -> Expression

simpl (AExpr vs e) | vs .==. 0  = E0
                   | vs .==. 1  = E1
                   | otherwise  = simp e

simp A0 = E0
simp A1 = E1
simp (AId v) = EId v
simp (AOp1 OP_SHR1 e) | getAnn e .<.  2 = E0
                      | getAnn e .==. 2 = E1
                      | otherwise = EOp1 OP_SHR1 (simpl e)
simp (AOp1 OP_SHR4 e) | getAnn e .<.  16 = E0
                      | getAnn e .==. 16 = E1
                      | otherwise = EOp1 OP_SHR4 (simpl e)
simp (AOp1 OP_SHR16 e) | getAnn e .<.  0x10000 = E0
                       | getAnn e .==. 0x10000 = E1
                       | otherwise = EOp1 OP_SHR16 (simpl e)
simp (AOp1 op e) = EOp1 op (simpl e)

simp (AOp2 op e1 e2) | op /= OP_AND && getAnn e2 .==. 0 = simpl e1
                     | op /= OP_AND && getAnn e1 .==. 0 = simpl e2
                     | op == OP_AND && (getAnn e2 .==. 0 || getAnn e2 .==. 0) = E0
                     | otherwise = EOp2 op (simpl e1) (simpl e2)

simp (AIf0 e1 e2 e3) | getAnn e1 .==. 0  = simpl e2
                     | getAnn e1 .>. 0   = simpl e3
                     | otherwise = EIf0 (simpl e1) (simpl e2) (simpl e3)
simp (AFold e1 e2 v1 v2 e3) = EFold (simpl e1) (simpl e2) v1 v2 (simpl e3)

