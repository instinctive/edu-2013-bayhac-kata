
module Bowling where

import Control.Applicative ((<*>), (<$>))
import Control.Monad (foldM)
import Control.Monad.Writer (Writer, runWriter, writer)
import Data.Char (intToDigit)
import Data.DList (DList, fromList, toList)
import Text.Printf (printf)

data Roll = Strike | Spare Int | One Int | Two Int deriving (Eq, Show)

toRoll :: Roll -> Int -> Roll
toRoll (One x)  y | x+y == 10 = Spare y
                  | otherwise = Two y
toRoll _       10             = Strike
toRoll _        x             = One x

toRolls :: [Int] -> [Roll]
toRolls xs = rs where rs = zipWith toRoll (Strike:rs) xs
                      
type Frame = [Roll]

frameTakeDrop :: [Roll] -> (Int, Int)
frameTakeDrop (Strike:_)    = (3, 1)
frameTakeDrop (_:Spare _:_) = (3, 2)
frameTakeDrop _             = (2, 2)

toFrames :: [Roll] -> [Frame]
toFrames = go 1 
  where
    go _ []               = []
    go 10 rs              = [rs]
    go n rs               = take x rs : go (n+1) (drop y rs)
      where (x, y) = frameTakeDrop rs

rollScore :: Roll -> Int
rollScore Strike    = 10
rollScore (Spare x) = x
rollScore (One x)   = x
rollScore (Two x)   = x

validFrame :: Frame -> Bool
validFrame [Strike,_      ,_] = True
validFrame [_     ,Spare _,_] = True
validFrame [One _ ,Two _  ]   = True
validFrame _ = False

frameScore :: Frame -> Maybe Int
frameScore rs | validFrame rs = Just $ sum $ map rollScore rs
              | otherwise     = Nothing

pinChar :: Int -> Char
pinChar 0 = '-'
pinChar x = intToDigit x

rollChar :: Roll -> Char
rollChar Strike    = 'X'
rollChar (Spare _) = '/'
rollChar (One x)   = pinChar x
rollChar (Two x)   = pinChar x

frameText :: Bool -> Frame -> String
frameText False (Strike:_) = "  |  | X|"
frameText final rs = printf " %c| %c| %c|" a b c
  where (a:b:c:_) = if final then cs else ' ' : cs
        cs = map rollChar rs ++ repeat ' '

type Output = (DList Char, DList Char)

bowlFrame :: (Maybe Int) -> (Bool, Frame) -> Writer Output (Maybe Int)
bowlFrame score (final, frame) = writer (score', output)
  where score' = (+) <$> score <*> (frameScore frame)
        output = mapBoth fromList (frameText final frame, printScore score')
        mapBoth f (a,b) = (f a, f b) -- why is this not already defined?
        printScore Nothing  = "        |"
        printScore (Just x) = printf "%8d|" x

bowlWriter :: [Frame] -> Writer Output (Maybe Int)
bowlWriter fs = foldM bowlFrame (Just 0) (zip (map (==10) [1..10]) fs')
  where fs' = fs ++ repeat []

-- | Given a raw list of pin scores, output a display of the complete
-- bowling display, as it might appear in an alley.
--
-- >>> putStr $ bowl [1,3,6,4,10,3,2,10,5,5,10,10,3,5,10,7,3]
-- +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
-- |  | 1| 3|  | 6| /|  |  | X|  | 3| 2|  |  | X|  | 5| /|  |  | X|  |  | X|  | 3| 5| X| 7| /|
-- |  +--+--+  +--+--+  +--+--+  +--+--+  +--+--+  +--+--+  +--+--+  +--+--+  +--+--+--+--+--+
-- |       4|      24|      39|      44|      64|      84|     107|     125|     133|     153|
-- +--------+--------+--------+--------+--------+--------+--------+--------+--------+--------+

bowl :: [Int] -> String
bowl xs = unlines 
  [ '+' : concat (replicate 10 "--+--+--+")
  , '|' : toList topline
  , '|' : concat (replicate  9 "  +--+--+") ++ "--+--+--+"
  , '|' : toList botline
  , '+' : concat (replicate 10 "--------+")
  ]
  where 
    (_, (topline, botline)) = runWriter.bowlWriter.toFrames.toRolls $ xs

