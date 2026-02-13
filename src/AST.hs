module AST where

type Symbol = Char
type WordLS = String
type Angle = Double
type Step = Double
type Iter = Int

-- Sistema base
data LSystem = LSys
  { name       :: String
  , axiom      :: WordLS
  , rules      :: [(Symbol, WordLS)]
  , angle      :: Angle
  , step       :: Step
  , iterations :: Iter
  }
  | Union LSystem LSystem
  | Interleave LSystem LSystem
  | Encapsulate LSystem
  | Inherit LSystem LSystem
  deriving Show

