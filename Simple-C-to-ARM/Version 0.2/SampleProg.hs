
module SampleProg (
        fac,
        fib,
        test
) where

import AST

test :: Prog
test = Fun "fac" [] (Seqn [])

fac                   :: Integer -> Prog
fac n                 =  Fun "fac" [] (Seqn [LocalVar "A", LocalVar "B",
                               Assign "A" (Val 1),
                               Assign "B" (Val n),
                               While (Var "B") (Seqn
                                  [Assign "A" (App Mul (Var "A") (Var "B")),
                                   Assign "B" (App Sub (Var "B") (Val 1))]),
                               Print (Var "A")])

fib                   :: Integer -> Prog
fib n                 =  Fun "fib" [] (Seqn [LocalVar "A", LocalVar "B", LocalVar "C", LocalVar "T",
                               Assign "A" (Val 1),
                               Assign "B" (Val 1),
                               Assign "C" (Val (n - 2)),
                               While (Var "C") (Seqn
                                  [Assign "T" (Var "B"),
                                   Assign "B" (App Add (Var "A") (Var "B")),
                                   Assign "A" (Var "T"),
                                   Assign "C" (App Sub (Var "C") (Val 1))]),
                               Print (Var "B")])