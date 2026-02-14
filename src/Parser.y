{
module Parser where
import AST
import Types
import Data.Char

}

%name parseLSystem
%tokentype { Token }
%error { parseError }

%token
  TLSystem      { TLSystem }
  TAxiom        { TAxiom }
  TRules        { TRules }
  TAngle        { TAngle }
  TStep         { TStep }
  TIterations   { TIterations }
  TUnion        { TUnion }
  TInterleave   { TInterleave }
  TEncap        { TEncap }
  TInherit      { TInherit }
  TArrow        { TArrow }
  TLBrace       { TLBrace }
  TRBrace       { TRBrace }
  TColon        { TColon }
  TSym         { TSym $$ }
  TId           { TId $$ }
  TNum          { TNum $$ }

%%

-- GRAMÁTICA

Program
  : LSystem                    { $1 }

LSystem
  : BaseSystem                 { $1 }
  | LSystem TUnion LSystem     { Union $1 $3 }
  | LSystem TInterleave LSystem { Interleave $1 $3 }
  | TEncap LSystem             { Encapsulate $2 }
  | LSystem TInherit LSystem   { Inherit $1 $3 }

BaseSystem
  : TLSystem TId TLBrace Body TRBrace
    { let (ax, rules, ang, st, it) = $4
      in LSys $2 ax rules ang st it }

Body
  : TAxiom TColon WordLS
    TRules TColon RuleList
    TAngle TColon TNum
    TStep TColon TNum
    TIterations TColon TNum
    { ($3, $6, $9, $12, round $15) }


RuleList
  : Rule                    { [$1] }
  | RuleList Rule           { $1 ++ [$2] }

Rule
  : TSym TArrow WordLS       { ($1, $3) }

WordLS
  : WordLS TSym   { $1 ++ [$2] }
  | TSym          { [$1] }


{
-- LEXER

lexer :: String -> [Token]
lexer [] = []

lexer (c:cs)
  | isSpace c = lexer cs

  -- izq de producciones---
  | isUpper c && (isArrow (dropWhile isSpace cs)) = TSym c : lexer cs   

  -- palabras reservadas o identificadores
  | isAlpha c =
      let (name, rest) = span isAlphaNum (c:cs)
      in keywordOrId name ++ lexer rest

  -- números
  | isDigit c =
      let (num, rest) = span (\x -> isDigit x || x == '.') (c:cs)
      in TNum (read num) : lexer rest

 -- flecha
  | c == '-' && not (null cs) && head cs == '>' =
      TArrow : lexer (tail cs)

  -- símbolos de la gramática
  | c == '{'  = TLBrace : lexer cs
  | c == '}'  = TRBrace : lexer cs
  | c == ':'  = TColon  : lexer cs
  | c == '+' = TSym c : lexer cs
  | c == '-' = TSym c : lexer cs
  | c == '[' = TSym c : lexer cs
  | c == ']' = TSym c : lexer cs
 

  | otherwise = error ("Unknown character: " ++ [c])

isArrow :: [Char] -> Bool
isArrow  ('-':'>':_) = True     -- -> etc--
isArrow _ = False

keywordOrId :: String -> [Token]
keywordOrId s =
  case s of
    "lsystem"     -> [TLSystem]
    "axiom"       -> [TAxiom]
    "rules"       -> [TRules]
    "angle"       -> [TAngle]
    "step"        -> [TStep]
    "iterations"  -> [TIterations]
    "union"       -> [TUnion]
    "interleave"  -> [TInterleave]
    "encap"       -> [TEncap]
    "inherit"     -> [TInherit]
    _ | all isUpper s -> TSym (head s) : lexer (tail s)
      |otherwise     -> [TId s]


parseString :: String -> LSystem
parseString = parseLSystem . lexer

parseError :: [Token] -> a
parseError toks =
  error ("Syntax error near tokens: " ++ show (take 10 toks))

}
