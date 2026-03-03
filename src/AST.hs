module AST where

import qualified Data.Map as M

type Symbol = Char
type WordLS = String
type Angle = Double
type Step = Double
type Iter = Int


-- Sistema base
data LSystem = LSys
  { name       :: String
  , axiom      :: WordLS
  , rules      :: M.Map Char Replacement
  , angle      :: Angle
  , step       :: Step
  , iterations :: Iter
  }
  | Union LSystem LSystem
  | Interleave LSystem LSystem
  deriving Show

-- Manejo de Encapsulado
data Replacement
  = Str String
  | Encaps LSystem
  deriving Show