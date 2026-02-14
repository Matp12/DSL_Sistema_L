module Eval where
import AST

applyRules :: [(Char,String)] -> String -> String
applyRules rs w = concatMap replace w
  where
    replace c = case lookup c rs of
                  Just r  -> r
                  Nothing -> [c]

iterateN :: Int -> (a -> a) -> a -> a
iterateN 0 _ x = x
iterateN n f x = iterateN (n-1) f (f x)

interleave :: String -> String -> String
interleave (x:xs) (y:ys) = x:y:interleave xs ys
interleave xs [] = xs
interleave [] ys = ys


merge_rules:: [(Char,String)] -> [(Char,String)] -> [(Char,String)]
merge_rules xs ys = let keys = map fst xs in
  xs ++ filter (\y-> not (fst y `elem` keys)) ys 

eval :: LSystem -> LSystem
eval (LSys n ax rs ang st it) =
  let final = iterateN it (applyRules rs) ax
  in LSys n final rs ang st it

eval (Union s1 s2) =
  let LSys n1 ax1 r1 a1 st1 it1 =  s1
      LSys n2 ax2 r2 a2 st2 it2 =  s2
  in eval  
        (LSys ((n1++" union ")++n2)
        (ax1 ++ ax2)
        (merge_rules r1 r2)
        a1 st1 (max it1 it2))

eval (Interleave s1 s2) =
  let LSys _ ax1 r1 a1 st1 it1 = eval s1
      LSys _ ax2 r2 a2 st2 it2 = eval s2
  in LSys "interleave"
     (interleave ax1 ax2)
     (r1 ++ r2)
     a1 st1 (max it1 it2)

eval (Encapsulate s) =
  let LSys n ax r a st it = eval s
  in LSys n ("[" ++ ax ++ "]") r a st it

eval (Inherit s1 s2) =
  let LSys _ ax1 r1 a1 st1 it1 = eval s1
      LSys _ ax2 r2 a2 st2 it2 = eval s2
  in LSys "inherit"
     ax1
     (r1 ++ r2)
     a1 st1 it1
