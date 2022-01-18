module Machine
(
        Vname,
        Val,
        State,
        Instr (..),
        Stack,
        Config,
        iexec,
        exec
) where

import Data.Map

--TODO Task 1.1
type Vname = String
--TODO Task 1.2
type Val = Int
--TODO Task 1.3
type State = Map Vname Val

--TODO Task 1.4
data Instr =
        LOADI Val
        | LOAD Vname
        | ADD
        | STORE Vname
        | JMP Val
        | JMPLESS Val
        | JMPGE Val

        deriving (Eq, Read, Show)

--TODO Task 1.5
type Stack = [Val]

--TODO Task 1.6
type Config = (Val, State, Stack)

--TODO Task 1.7
iexec :: Instr -> Config -> Config
iexec (LOADI x) (val, state, stack) = (val+1, state, x:stack) --x:stack adds x to the start of the list (aka stack)
iexec (LOAD v) (val, state, stack) = (val+1, state, state!v:stack) --state!v gets val from 'v' state and adds to stack
iexec ADD (val, state, stack) = (val+1, state, head stack+stack!!1:Prelude.drop 2 stack) --adds the 0th and 1th element together and to stack and removes them from stack
iexec (STORE v) (val, state, stack) = (val+1, insert v (head stack) state, Prelude.drop 1 stack) --drops first value from stack after storing it in the specified variable
iexec (JMP i) (val, state, stack) = (val+i+1, state, stack) --adds i to program counter and increments by 1
iexec (JMPLESS i) (val, state, stack) = (if head stack>stack!!1 then val+i+1 else val+1, state, Prelude.drop 2 stack) --compares 2 values and adds the correct value to program counter
iexec (JMPGE i) (val, state, stack) = (if head stack<=stack!!1 then val+i+1 else val+1, state, Prelude.drop 2 stack) --compares 2 values and adds correct value to program counter

--TODO Task 1.8
exec :: [Instr] -> Config -> Config
exec [] (val, state, stack) = (val, state, stack)
exec (x:xs) (val, state, stack) = exec xs (iexec x (val, state, stack))