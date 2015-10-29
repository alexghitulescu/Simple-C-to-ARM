
module SampleProg (
        fac,
        fib
) where

import AST


fac                   :: Integer -> Prog
fac n                 =  Seqn [NewVar "A", NewVar "B",
                               Assign "A" (Val 1),
                               Assign "B" (Val n),
                               While (Var "B") (Seqn
                                  [Assign "A" (App Mul (Var "A") (Var "B")),
                                   Assign "B" (App Sub (Var "B") (Val 1))]),
                               Print (Var "A")]

fib                   :: Integer -> Prog
fib n                 =  Seqn [NewVar "A", NewVar "B", NewVar "C", NewVar "T",
                               Assign "A" (Val 1),
                               Assign "B" (Val 1),
                               Assign "C" (Val (n - 2)),
                               While (Var "C") (Seqn
                                  [Assign "T" (Var "B"),
                                   Assign "B" (App Add (Var "A") (Var "B")),
                                   Assign "A" (Var "T"),
                                   Assign "C" (App Sub (Var "C") (Val 1))]),
                               Print (Var "B")]