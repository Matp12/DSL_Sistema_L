module Types where

data Token
  = TLSystem
  | TAxiom
  | TRules
  | TAngle
  | TStep
  | TIterations
  | TUnion
  | TInterleave
  | TEncap
  | TArrow
  | TLBrace
  | TRBrace
  | TLParen
  | TRParen
  | TColon
  | TSemicolon
  | TPlus 
  | TMinus 
  | TLBracket 
  | TRBracket 
  | TSym Char
  | TId String
  | TWord String
  | TNum Double
  deriving Show
