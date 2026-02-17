module Eval where
import AST
import qualified Data.Map as M

applyRules :: M.Map Char Replacement -> String -> String
applyRules rs w = concatMap replace w
  where
    replace c =
      case M.lookup c rs of
        Just (Str r)   -> r
        Just (Encaps g) -> "[" ++ axiom (eval g) ++ "]"
        Nothing        -> [c]

iterateN :: Int -> (a -> a) -> a -> a
iterateN 0 _ x = x
iterateN n f x = iterateN (n-1) f (f x)

interleave :: String -> String -> String
interleave (x:xs) (y:ys) = x:y:interleave xs ys
interleave xs [] = xs
interleave [] ys = ys


eval :: LSystem -> LSystem
eval (LSys n ax rs ang st it) =
  let final = iterateN it (applyRules rs) ax
  in LSys n final rs ang st it

eval (Union s1 s2) =
 eval  
    (LSys ((name s1++" union ")++name s2)
    ((axiom s1) ++ (axiom s2))
    (M.union (rules s1) (rules s2))
    (angle s1) (step s1) (max (iterations s1) (iterations s1)))

eval (Interleave s1 s2) =
  eval 
    (LSys ("interleave "++ (name s1)++ " "++ (name s2))
     (interleave (axiom s1) (axiom s2))
     (M.union (rules s1) (rules s2))
     (angle s1) (step s1) (max (iterations s1) (iterations s1)))
