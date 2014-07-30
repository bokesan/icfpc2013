module Main (main) where

import Data.Word
import Numeric (showHex)
import System.Environment (getArgs)

import Program
import Generator
import Bonus
import Eval
import Parser

main :: IO ()
main = getArgs >>= mainFun

mainFun :: [String] -> IO ()
mainFun ["gen", size, ops] = gen size ops False ""
mainFun ["bonus", size, ops, samples] = gen size ops True samples
mainFun ["eval", prog, val] = printHex (eval (parse prog) (read val))
mainFun ["evalfile", file, val] = evalFile file (read val)
mainFun _ = do putStrLn "usage: program gen size ops"
               putStrLn "       program eval program value"

gen :: String -> String -> Bool -> String -> IO ()
gen size ops bonus samp =
   let size' = read size
       (ops1,ops2,ops3) = parseOps ops
       samples = parseSamples samp
   in
      mapM_ (\p -> putStrLn (show p))
            (if bonus
             then (genBonusPrograms ops1 ops2 size' samples)
             else (genPrograms ops1 ops2 ops3 size'))

printHex :: Word64 -> IO ()
printHex n = do putStr "0x"
                putStrLn (showHex n "")

evalFile :: String -> Word64 -> IO ()
evalFile file val = do s <- readFile file
                       mapM_ (testProg val) (lines s)

testProg :: Word64 -> String -> IO ()
testProg val p = let prog = parse p in
                 do putStr (show prog)
                    putStr "\t"
                    printHex (eval prog val)


parse :: String -> Program
parse prog = case parseProgram prog of
               Right p -> p
               Left e -> error ("parse error: " ++ show e)


parseOps :: String -> ([Op1], [Op2], (Bool, Bool, Bool))
parseOps s = foldr scanOpt ([], [], (False, False, False)) (splitOn (','==) s)

scanOpt :: String -> ([Op1], [Op2], (Bool, Bool, Bool)) -> ([Op1], [Op2], (Bool, Bool, Bool))
scanOpt "fold"  (ops1,ops2,(if0,_,tfold)) = (ops1,ops2,(if0, True, tfold))
scanOpt "tfold" (ops1,ops2,(if0,fold,_)) = (ops1,ops2,(if0, fold, True))
scanOpt "if0"   (ops1,ops2,(_,fold,tfold)) = (ops1,ops2,(True, fold, tfold))
scanOpt "not"   (ops1,ops2,ops3) = (OP_NOT : ops1, ops2, ops3)
scanOpt "shl1"   (ops1,ops2,ops3) = (OP_SHL1 : ops1, ops2, ops3)
scanOpt "shr1"   (ops1,ops2,ops3) = (OP_SHR1 : ops1, ops2, ops3)
scanOpt "shr4"   (ops1,ops2,ops3) = (OP_SHR4 : ops1, ops2, ops3)
scanOpt "shr16"  (ops1,ops2,ops3) = (OP_SHR16 : ops1, ops2, ops3)
scanOpt "and"    (ops1,ops2,ops3) = (ops1, OP_AND : ops2, ops3)
scanOpt "or"     (ops1,ops2,ops3) = (ops1, OP_OR : ops2, ops3)
scanOpt "xor"    (ops1,ops2,ops3) = (ops1, OP_XOR : ops2, ops3)
scanOpt "plus"   (ops1,ops2,ops3) = (ops1, OP_PLUS : ops2, ops3)
scanOpt "bonus"  ops = ops
scanOpt opt _ = error ("unknow option: " ++ opt)

splitOn :: (Char -> Bool) -> String -> [String]
splitOn p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : splitOn p s''
                            where (w, s'') = break p s'

parseSamples :: String -> [(Word64,Word64)]
parseSamples s = map parseSample (splitOn (','==) s)

parseSample :: String -> (Word64, Word64)
parseSample s = let [input, output] = splitOn ('='==) s in
                 (read input, read output)

