module Eval where
import AST
import qualified Data.Map as M
import Control.Monad.Writer
import Control.Parallel.Strategies

applyRules :: M.Map Char Replacement -> String -> String
applyRules rs w =
    foldr (++) "" $ parMap rdeepseq replace w
  where
    replace c =
      case M.lookup c rs of
        Just (Str r)    -> r
        Just (Encaps g) -> "[" ++ last (snd (runWriter (evalM g))) ++ "]"
        Nothing         -> [c]


stepM :: M.Map Char Replacement -> String -> Writer [String] String
stepM rs w = do
    let w' = applyRules rs w
    tell [w']
    return w'


iterateNM :: Int -> (a -> Writer [a] a) -> a -> Writer [a] a
iterateNM 0 _ x = return x
iterateNM n f x = f x >>= iterateNM (n-1) f

interleave :: String -> String -> String
interleave (x:xs) (y:ys) = x:y:interleave xs ys
interleave xs [] = xs
interleave [] ys = ys

evalM :: LSystem -> Writer [String] LSystem
evalM (LSys n ax rs ang st it) = do
    tell [ax]
    final <- iterateNM it (stepM rs) ax
    return (LSys n final rs ang st it)

evalM (Union s1 s2) =
  evalM  
    (LSys ((name s1++" union ")++name s2)
    ((axiom s1) ++ (axiom s2))
    (M.union (rules s1) (rules s2))
    (angle s1) (step s1) (max (iterations s1) (iterations s1)))

evalM (Interleave s1 s2) =
    evalM 
      (LSys ("interleave "++ (name s1)++ " "++ (name s2))
      (interleave (axiom s1) (axiom s2))
      (M.union (rules s1) (rules s2))
      (angle s1) (step s1) (max (iterations s1) (iterations s1)))
