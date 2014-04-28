
module SampleProg (
        fac,
        fib,
        test,
        test2
) where

import AST

test                  :: Prog
test                  = PSeq[
                        GlobalVar "A",
                        Fun "main" [] (Ex (Apply "test" [])),
                        Fun "test" [] (Seqn [Assign "A" (App Mul (Val 5) (Val 5))])
                        ]

test2                 :: Prog
test2                 = PSeq [
                        GlobalVar "result",
                        Fun "fib" ["i"] (Seqn [
                                LocalVar "a",
                                LocalVar "b",
                                LocalVar "c",
                                LocalVar "t",
                                Assign "a" (Val 1),
                                Assign "b" (Val 1),
                                Assign "c" (App Sub (Val 16) (Val 2)),
                                While (Var "c") (Seqn [
                                        Assign "t" (Var "b"),
                                        Assign "b" (App Add (Var "a") (Var "b")),
					Assign "a" (Var "t"),
					Assign "c" (App Sub (Var "c") (Val 1))]),
                                Assign "result" (Var "b")]),
                        Fun "main" [] (Seqn [
                                Ex (Apply "fib" [Val 16]),
                                Print(Var "result")])]
                        
fac                   :: Integer -> Prog
fac n                 = PSeq[
                        Fun "main" [] (Ex (Apply "fac" [Val n])),
                        Fun "fac" ["B"] (Seqn [LocalVar "A",
                               Assign "A" (Val 1),
                               While (Var "B") (Seqn
                                  [Assign "A" (App Mul (Var "A") (Var "B")),
                                   Assign "B" (App Sub (Var "B") (Val 1))]),
                               Print (Var "A")])
                        ]

fib                   :: Integer -> Prog
fib n                 = PSeq[ 
                        Fun "fib" [] (Seqn [LocalVar "A", LocalVar "B", LocalVar "C", LocalVar "T",
                               Assign "A" (Val 1),
                               Assign "B" (Val 1),
                               Assign "C" (App Sub (Val n) (Val 2)),
                               While (Var "C") (Seqn
                                  [Assign "T" (Var "B"),
                                   Assign "B" (App Add (Var "A") (Var "B")),
                                   Assign "A" (Var "T"),
                                   Assign "C" (App Sub (Var "C") (Val 1))]){-,
                               Print (Var "B")-}]),
                        Fun "main" [] (Seqn [Ex (Apply "fib" []), Print (Var "B")])
                        ]