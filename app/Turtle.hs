module Turtle where

import Graphics.Gloss

import Control.Monad.State

type Angle = Float

data Turtle = Turtle
  { pos   :: Point
  , dir   :: Angle
  , stack :: [Turtle]
  }

stepT :: Float -> Float -> Char -> State Turtle [((Float,Float),(Float,Float))]
stepT len ang c = do
    t <- get
    case c of
        'F' -> do
            let (x,y) = pos t
                a = dir t
                x' = x + len * cos a
                y' = y + len * sin a
            put t{pos=(x',y')}
            return [((x,y),(x',y'))]

        '+' -> put t{dir = dir t - ang} >> return []
        '-' -> put t{dir = dir t + ang} >> return []

        '[' -> put t{stack = t : stack t} >> return []

        ']' -> case stack t of
                 (s:ss) -> put s{stack=ss} >> return []
                 []     -> return []

        _ -> return []

interpret :: Float -> Float -> String -> [((Float,Float),(Float,Float))]
interpret len ang str =
    evalState (fmap concat (mapM (stepT len ang) str))
              (Turtle (0,-300) (pi/2) [])

drawSegments :: [((Float,Float),(Float,Float))] -> Picture
drawSegments segs =
    Pictures [ Line [p1,p2] | (p1,p2) <- segs ]

pictureFromWord :: Float -> Float -> String -> Picture
pictureFromWord st ang word =
    drawSegments (interpret st (ang*pi/180) word)

animateTrace :: Float -> Float -> [String] -> IO ()
animateTrace step ang trace =
    animate
        (InWindow "L-System" (900,900) (20,20))
        black
        frame
  where
    pics = map (pictureFromWord step ang) trace

    frame t =
        let i = floor (t*0.8) `mod` length pics
        in color white (pics !! i)
