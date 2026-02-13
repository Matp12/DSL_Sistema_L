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
  | TInherit
  | TArrow
  | TLBrace
  | TRBrace
  | TColon
  | TSym Char
  | TId String
  | TWord String
  | TNum Double
  deriving Show
