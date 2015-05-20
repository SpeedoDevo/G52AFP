> import Data.Ord
> import Data.List
> import Control.Monad
> import Control.Applicative
> import Control.Monad.Writer

G52AFP Coursework 2 - Monadic Compiler

Barnabas Forgo, Roshan Hunjan  
psybf@nottingham.ac.uk, psyrh3@nottingham.ac.uk

Best viewed on https://gist.github.com/7c69b324be0a62f6acd6

--------------------------------------------------------------------------------

Imperative language:

>
> data Prog             =  Assign Name Expr
>                       |  If Expr Prog Prog
>                       |  While Expr Prog
>                       |  Seqn [Prog]
>                          deriving Show
>
> data Expr             =  Val Int | Var Name | App Op Expr Expr
>                          deriving Show
>
> type Name             =  Char
>
> data Op               =  Add | Sub | Mul | Div | Gt
>                          deriving Show

Factorial example:


> fac                   :: Int -> Prog
> fac n                 =  Seqn [Assign 'A' (Val 1),
>                                Assign 'B' (Val n),
>                                While (Var 'B') (Seqn
>                                   [Assign 'A' (App Mul (Var 'A') (Var 'B')),
>                                    Assign 'B' (App Sub (Var 'B') (Val (1)))])]

Virtual machine:

>
> type Stack            =  [Int]
>
> type Mem              =  [(Name,Int)]
>
> type Code             =  [Inst]
>
> data Inst             =  PUSH Int
>                       |  PUSHV Name
>                       |  POP Name
>                       |  DO Op
>                       |  JUMP Label
>                       |  JUMPZ Label
>                       |  LABEL Label
>                          deriving Show
>
> type Label            =  Int

State monad:

>
> type State            =  Label
>
> data ST a             =  S (State -> (a, State))
>
> apply                 :: ST a -> State -> (a,State)
> apply (S f) x         =  f x
>
> instance Monad ST where
>    -- return          :: a -> ST a
>    return x           =  S (\s -> (x,s))
>
>    -- (>>=)           :: ST a -> (a -> ST b) -> ST b
>    st >>= f           =  S (\s -> let (x,s') = apply st s in apply (f x) s')
>
> instance Functor ST where
>    fmap               = liftM
> instance Applicative ST where
>    pure               = return
>    (<*>)              = ap
>
> fresh                 :: ST Int
> fresh                 =  S (\n -> (n, n + 1))


lfresh is just a lifted version of fresh so that it works with WriterT

>
> lfresh                :: WriterT Code ST Int
> lfresh                =  lift fresh


--------------------------------------------------------------------------------

COMPILER
========

Simple pattern matching to compile an Expr to Code.

> compExpr              :: Expr -> Code
> compExpr (Val x)      =  [PUSH x]
> compExpr (Var x)      =  [PUSHV x]
> compExpr (App op x y) =  compExpr x ++ compExpr y ++ [DO op]


Compiles a program by accumulating Code in a WriterT monad using tells.

> compProg              :: Prog -> WriterT Code ST ()

Compile the expr and pop it to a variable. This works because compExpr leaves
the result of Expr on the top of the stack.

> compProg (Assign n e) =  do tell $ compExpr e
>                             tell [POP n]

If follows this sequence.  
[ --execute e (result on top of stack),  
  JUMPZ L1,  
  --execute p,  
  JUMP L2,  
  LABEL L1,  
  --execute q,  
  LABEL L2]  

> compProg (If e a b)   =  do tell $ compExpr e
>                             l1 <- lfresh
>                             tell [JUMPZ l1]
>                             compProg a
>                             l2 <- lfresh
>                             tell [JUMP l2, LABEL l1]
>                             compProg b
>                             tell [LABEL l2]

While is similar to If.

[ LABEL L1,  
  --execute e,  
  JUMPZ L2,  
  -- execute p,  
  JUMP L1,  
  LABEL L2]  

> compProg (While e p)  =  do l1 <- lfresh
>                             tell [LABEL l1]
>                             tell $ compExpr e
>                             l2 <- lfresh
>                             tell [JUMPZ l2]
>                             compProg p
>                             tell [JUMP l1, LABEL l2]

Each of Seqn's components are just compiled and appended together.

> compProg (Seqn [])    =  return ()
> compProg (Seqn (p:ps))=  do compProg p
>                             compProg (Seqn ps)

Wrapper for compProg, which actually "runs" it and extracts the result.

> comp                  :: Prog -> Code
> comp p                =  snd . fst $ apply (runWriterT $ compProg p) 0

--------------------------------------------------------------------------------

RUNTIME
=======

New tyes to handle execution.

> type Pos              = Int
> data Frame            = Frame {pos :: Pos, c :: Code, s :: Stack, m :: Mem}

step represents a single step of program execution. It loops through each Inst
in the Code and calls eval to evaluate it. If it reached the end of the code,
then it just returns the memory.

> step                  :: Frame -> Mem
> step f                =  if (pos f) < (length $ c f) then
>                             step $ eval f
>                          else
>                             m f

Evaluate a single Inst.

> eval                  :: Frame -> Frame
> eval (Frame pos c s m)=  case (c !! pos) of

Pushes a value on the stack by appending it to s :: Stack.

>                               PUSH x  -> (Frame (pos+1) c (x:s) m)

Pushes a variable's value on the stack using pushV.

>                               PUSHV x -> (Frame (pos+1) c ((pushV m x):s) m)

Takes the top of the Stack and saves it in Mem using pop.

>                               POP x   -> (Frame (pos+1) c (tail s) (pop x s m))

Executes an operation on the Stack using execDo.

>                               DO x    -> (Frame (pos+1) c (execDo x s) m)

Jumps to a labeled location in the code using jump.

>                               JUMP x  -> (Frame (jump 0 c x) c s m)

If the top of the stack is 0 then just continues execution,
otherwise it jumps to the specified label.

>                               JUMPZ x -> if (head s) /= 0 then
>                                             (Frame (pos+1) c s m)
>                                          else
>                                             (Frame (jump 0 c x) c s m)

Just a LABEL, continue execution with the next Inst.

>                               LABEL x -> (Frame (pos+1) c s m)

Filter deletes the existing variable in Mem and we add it back with the new value.

> pop                   :: Name -> Stack -> Mem -> Mem
> pop n s m             =  (n, head s) : (filter (\(a, _) -> a /= n) m)

Loops through the Mem looking for a variable name, if it is found then it returns the value of it.

> pushV                 :: Mem -> Name -> Int
> pushV ((v, i):ms) n   |  v == n    = i
>                       |  otherwise = pushV ms n

Loops through the Code looking for a specific LABEL starting from pos,
if it's found then it returns the Pos of it.

> jump                  :: Pos -> Code -> Label -> Pos
> jump pos c l1         =  case (c !! pos) of
>                               LABEL l2 -> if l1 == l2 then pos else jump (pos+1) c l1
>                               _        -> jump (pos+1) c l1

Performs a specific operation on the Stack, returning a new one when finished.
Added Gt to extend the language (and test the compiler).

> execDo                :: Op -> Stack -> Stack
> execDo Add (x:y:xs)   =  (y + x) : xs
> execDo Sub (x:y:xs)   =  (y - x) : xs
> execDo Mul (x:y:xs)   =  (y * x) : xs
> execDo Div (x:y:xs)   =  (y `div` x) : xs
> execDo Gt  (x:y:xs)   =  if (y < x) then 0:xs else 1:xs

Wrapper that starts step for executing the code. It sorts the
returned memory by variable name.

> exec                  :: Code -> Mem
> exec code             =  sortBy (comparing fst) $ step (Frame 0 code [] [])

--------------------------------------------------------------------------------

TESTS
=====

Test programs for the compiler.

sump sums a list of Ints

> sump :: [Int] -> Prog
> sump xs = Seqn ([Assign 'a' (Val 0)] ++ [Assign 'a' (App Add (Val x) (Var 'a')) | x <- xs])

gcdp finds the greatest common divisor of two Ints, using the Euclidean algorithm.
The languege needed extension by Gt to implement this.

> gcdp :: Int -> Int -> Prog
> gcdp a b = Seqn [Assign 'a' (Val a), Assign 'b' (Val b),
>                   While (App Sub (Var 'a') (Var 'b'))
>                      (Seqn [If (App Gt (Var 'a') (Var 'b'))
>                           (Assign 'a' (App Sub (Var 'a') (Var 'b')))
>                           (Assign 'b' (App Sub (Var 'b') (Var 'a')))])]
