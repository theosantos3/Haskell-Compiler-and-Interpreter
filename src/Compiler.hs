module Compiler
(
    acomp,
    bcomp,
    ccomp
) where

import Machine
import Interpreter

--TODO Task 3.1
acomp :: AExp -> [Instr]
acomp (N x) = [LOADI x]
acomp (V v) = [LOAD v]
acomp (Plus a1 a2) = acomp a1 ++ acomp a2 ++ [ADD]

--TODO Task 3.2
bcomp :: BExp -> Bool -> Int -> [Instr]
bcomp (Bc b) f i = [JMP i | b==f]
bcomp (Not b) f i = bcomp b (not f) i
bcomp (And b1 b2) f i = do
                            let cb2 = bcomp b2 f i;
                            let m = if f then length cb2 else length cb2 + i;
                            let cb1 = bcomp b1 False m in cb1 ++ cb2
bcomp (Less a1 a2) f i = acomp a1 ++ acomp a2 ++ if f then [JMPLESS i] else [JMPGE i]


--TODO Task 3.3
ccomp :: Com -> [Instr]
ccomp (Assign v a) = acomp a ++ [STORE v]
ccomp (Seq c1 c2) = ccomp c1 ++ ccomp c2
ccomp (If b c1 c2) = do
                        let cc1 = ccomp c1;
                        let cc2 = ccomp c2;
                        let cb = bcomp b False (length cc1 + 1) in cb ++ cc1 ++ [JMP (length cc2)] ++ cc2
ccomp (While b c) = do 
                        let cc = ccomp c;
                        let cb = bcomp b False (length cc + 1) in cb ++ cc ++ [JMP (- (length cb + length cc + 1))]
ccomp SKIP = []